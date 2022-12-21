--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Spawn.Internal;
with Spawn.Posix;
with GNAT.OS_Lib;

package body Spawn.Channels is

   use all type Spawn.Common.Pipe_Kinds;

   To_Event_Set                : constant array (Spawn.Common.Pipe_Kinds) of
     Spawn.Polls.Watch_Event_Set :=
       (Spawn.Common.Stdin => Spawn.Polls.Output,
        others             => Spawn.Polls.Input);

   function Errno return Interfaces.C.int is
     (Interfaces.C.int (GNAT.OS_Lib.Errno));
   --  return errno, number of last error

   -----------------------------
   -- Close_Child_Descriptors --
   -----------------------------

   procedure Close_Child_Descriptors
     (Self    : in out Channels;
      Success : out Boolean)
   is
      use type Interfaces.C.int;
      procedure Close (Pipe : in out Interfaces.C.int);

      -----------
      -- Close --
      -----------

      procedure Close (Pipe : in out Interfaces.C.int) is
         Error : Interfaces.C.int;
      begin
         if Pipe /= Invalid then
            Error := Spawn.Posix.close (Pipe);
            Pipe := Invalid;

            if Error /= 0 then
               Success := False;
            end if;
         end if;
      end Close;

   begin
      Success := True;

      for Pipe of Self.Child loop
         Close (Pipe);
      end loop;

      --  This breaks the test: Close (Self.PTY_Slave);
   end Close_Child_Descriptors;

   procedure Close_Parent_Descriptor
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Poll : Spawn.Polls.Poll_Access)
   is
      use type Interfaces.C.int;

      Ignore : Interfaces.C.int;
      Pipe   : Interfaces.C.int renames Self.Parent (Kind);
   begin
      if Pipe /= Invalid then
         Poll.Watch (Value => Pipe, Events => Spawn.Polls.Empty_Set);
         Ignore := Spawn.Posix.close (Pipe);
         Pipe := Invalid;
      end if;
   end Close_Parent_Descriptor;

   ------------------------------
   -- Close_Parent_Descriptors --
   ------------------------------

   procedure Close_Parent_Descriptors
     (Self    : in out Channels;
      Success : out Boolean)
   is
      use type Interfaces.C.int;
      procedure Close (Pipe : in out Interfaces.C.int);

      -----------
      -- Close --
      -----------

      procedure Close (Pipe : in out Interfaces.C.int) is
         Error : Interfaces.C.int;
      begin
         if Pipe /= Invalid then
            Error := Spawn.Posix.close (Pipe);
            Pipe := Invalid;

            if Error /= 0 then
               Success := False;
            end if;
         end if;
      end Close;
   begin
      Success := True;

      for Pipe of Self.Parent loop
         if Pipe /= Self.PTY_Master then
            Close (Pipe);
         else
            Pipe := Invalid;
         end if;
      end loop;

      Close (Self.PTY_Master);
   end Close_Parent_Descriptors;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Channels;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set)
   is
      use all type Spawn.Polls.Event;
      use type Spawn.Polls.Descriptor;
   begin
      if Value = Self.Parent (Launch) then
         if Events (Input) then
            declare
               use type Ada.Streams.Stream_Element_Offset;
               use type Interfaces.C.size_t;

               Count      : Interfaces.C.size_t;
               errno      : Integer := 0;
               Error_Data : Ada.Streams.Stream_Element_Array
                 (1 .. errno'Size / 8)
                 with Import, Convention => Ada, Address => errno'Address;
            begin
               Count := Posix.read (Value, Error_Data, Error_Data'Length);

               if Count = Error_Data'Length then
                  Self.Process.Exit_Code := Process_Exit_Code (errno);
               end if;
            end;
         end if;

         if Events (Close) or Events (Error) then
            if Self.Process.Exit_Code = Process_Exit_Code'Last then
               Self.Process.Status := Running;
               Self.Process.Emit_Started;
               Self.Process.Emit_Stdin_Available;

            else
               Self.Process.Emit_Error_Occurred
                 (Integer (Self.Process.Exit_Code));
            end if;
         end if;

         declare
            Error : constant Interfaces.C.int := Posix.close (Value);
         begin
            if Error /= 0 then
               Self.Process.Emit_Error_Occurred (Integer (Error));
            end if;
         end;
      else
         --  Stdin, Stdout and Stderr could share the same FD for TTY, so
         --  check them all in conbinations
         if Value = Self.Parent (Stdin) and Events (Output) then
            Self.Process.Emit_Stdin_Available;
         end if;

         if Events (Input) then
            if Value = Self.Parent (Stdout) then
               Self.Process.Emit_Stdout_Available;
            elsif Value = Self.Parent (Stderr) then
               Self.Process.Emit_Stderr_Available;
            end if;
         end if;
      end if;
   end On_Event;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Interfaces.C.size_t;

      Count : constant Interfaces.C.size_t :=
        Posix.read (Self.Parent (Kind), Data, Data'Length);

      Error : constant Interfaces.C.int := Errno;
   begin
      Last := Data'First - 1;

      if Count /= Interfaces.C.size_t'Last then
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
      elsif Error not in Posix.EAGAIN | Posix.EINTR then
         Self.Process.Emit_Error_Occurred (Integer (Error));
      end if;
   end Read;

   --------------------
   -- Setup_Channels --
   --------------------

   procedure Setup_Channels
     (Self    : in out Channels;
      Use_PTY : Common.Pipe_Flags;
      Child   : out Pipe_Array;
      Success : out Boolean)
   is
      use type Interfaces.C.int;

      procedure Setup_Pipe
        (Kind    : Posix.Pipe_Ends;
         Read    : out Interfaces.C.int;
         Write   : out Interfaces.C.int;
         Success : in out Boolean);

      procedure Setup_PTY (Success : in out Boolean);

      ----------------
      -- Setup_Pipe --
      ----------------

      procedure Setup_Pipe
        (Kind    : Posix.Pipe_Ends;
         Read    : out Interfaces.C.int;
         Write   : out Interfaces.C.int;
         Success : in out Boolean)
      is

         procedure Cleanup;
         --  Close file descriptors and unreference channel.

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
            Ignore : Interfaces.C.int;

         begin
            Ignore := Spawn.Posix.close (Read);
            Ignore := Spawn.Posix.close (Write);

            Read  := Invalid;
            Write := Invalid;
         end Cleanup;

         Fds   : Spawn.Posix.Fd_Pair;

      begin
         Read  := Invalid;
         Write := Invalid;

         if not Success then
            return;
         end if;

         --  Create pipe

         if Spawn.Posix.pipe2 (Fds, Posix.O_CLOEXEC) /= 0 then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         Read := Fds (Spawn.Posix.Read_End);
         Write := Fds (Spawn.Posix.Write_End);

         --  Setup non-blocking mode for parent's end

         if Posix.fcntl
           (Fds (Kind), Posix.F_SETFL, Posix.O_NONBLOCK) /= 0
         then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Cleanup;
            Success := False;

            return;
         end if;
      end Setup_Pipe;

      ---------------
      -- Setup_PTY --
      ---------------

      procedure Setup_PTY (Success : in out Boolean) is
         procedure Cleanup;

         PTY_Master : Interfaces.C.int renames Self.PTY_Master;
         Status     : Interfaces.C.int;
         Slave_Name : Interfaces.C.char_array (1 .. 64);

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
            Ignore : Interfaces.C.int;

         begin
            if PTY_Master /= Invalid then
               Ignore := Spawn.Posix.close (PTY_Master);
               PTY_Master := Invalid;
            end if;

            if Self.PTY_Slave /= Invalid then
               Ignore := Spawn.Posix.close (Self.PTY_Slave);
               Self.PTY_Slave := Invalid;
            end if;
         end Cleanup;

      begin
         PTY_Master := Invalid;
         Self.PTY_Slave  := Invalid;

         if not Success then
            return;
         end if;

         --  Open pseudoterminal's master descriptor

         PTY_Master := Spawn.Posix.posix_openpt
           (Spawn.Posix.O_RDWR + Spawn.Posix.O_NOCTTY);

         if PTY_Master = Invalid then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Mark file descriptor as be closed on exec

         if Spawn.Posix.fcntl
           (PTY_Master, Spawn.Posix.F_SETFD, Spawn.Posix.FD_CLOEXEC) = Invalid
         then
            Cleanup;
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Change mode and owner of the slave pseudoterminal device

         if Spawn.Posix.grantpt (PTY_Master) /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Unlock slave pseudoterminal device

         if Spawn.Posix.unlockpt (PTY_Master) /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Get name of the slave pseudoterminal device

         Status := Spawn.Posix.ptsname_r
           (PTY_Master, Slave_Name, Slave_Name'Length);

         if Status /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred (Integer (Status));
            Success := False;

            return;
         end if;

         --  Open slave device

         Self.PTY_Slave := Spawn.Posix.open
           (Slave_Name, Spawn.Posix.O_RDWR + Spawn.Posix.O_CLOEXEC, 0);

         if Self.PTY_Slave = Invalid then
            Cleanup;
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Setup non-blocking mode for the channel

         if Posix.fcntl
           (PTY_Master, Posix.F_SETFL, Posix.O_NONBLOCK) /= 0
         then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Cleanup;
            Success := False;

            return;
         end if;
      end Setup_PTY;

      use type Spawn.Common.Pipe_Flags;
   begin
      Success := True;

      if Use_PTY /= (Use_PTY'Range => False) then
         Setup_PTY (Success);
         Child := (Child'Range => Self.PTY_Slave);
      end if;

      if Use_PTY (Stdin) then
         Self.Parent (Stdin) := Self.PTY_Master;
      else
         Setup_Pipe
           (Kind    => Spawn.Posix.Write_End,
            Read    => Self.Child (Stdin),
            Write   => Self.Parent (Stdin),
            Success => Success);

         Child (Stdin) := Self.Child (Stdin);
      end if;

      for X in Launch .. Stderr loop
         if X /= Launch and then Use_PTY (X) then
            Self.Parent (X) := Self.PTY_Master;
         else
            Setup_Pipe
              (Kind    => Spawn.Posix.Read_End,
               Read    => Self.Parent (X),
               Write   => Self.Child (X),
               Success => Success);

            Child (X) := Self.Child (X);
         end if;
      end loop;
   end Setup_Channels;

   -----------------
   -- Start_Watch --
   -----------------

   procedure Start_Watch
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Poll : Spawn.Polls.Poll_Access)
   is
      Pipe : Interfaces.C.int renames Self.Parent (Kind);
   begin
      Poll.Watch
        (Value    => Pipe,
         Events   => To_Event_Set (Kind),
         Listener => Self'Unchecked_Access);
   end Start_Watch;

   procedure Write_Stdin
     (Self : in out Channels;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Interfaces.C.size_t;

      Count : constant Interfaces.C.size_t :=
        Posix.write (Self.Parent (Stdin), Data, Data'Length);

      Error : constant Interfaces.C.int := Errno;
   begin
      Last := Data'First - 1;

      if Count /= Interfaces.C.size_t'Last then
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
      elsif Error not in Posix.EAGAIN | Posix.EINTR then
         Self.Process.Emit_Error_Occurred (Integer (Error));
      end if;
   end Write_Stdin;
end Spawn.Channels;
