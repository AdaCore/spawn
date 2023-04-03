--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Warnings (Off);
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (On);
--  GNAT: different versions of compiler use different pragmas to enable
--  Ada 2022 features.

with Interfaces.C;

with GNAT.OS_Lib;

with Glib.Error;

with Spawn.Internal;
with Spawn.Posix;

package body Spawn.Channels is

   use type Ada.Streams.Stream_Element_Offset;
   use type Glib.Gboolean;
   use type Glib.IOChannel.Giochannel;
   use type Glib.IOChannel.GIOCondition;
   use type Glib.Main.G_Source_Id;
   use type Interfaces.C.int;

   function Add_Watch is new Glib.IOChannel.Generic_Add_Watch
     (User_Data => Spawn.Internal.Process_Reference);

   function On_Stdin_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
     with Convention => C;

   function On_Stdout_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
     with Convention => C;

   function On_Stderr_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
     with Convention => C;

   procedure Start_Stdin_Watch (Self : in out Channels);

   procedure Start_Stdout_Watch (Self : in out Channels);

   procedure Start_Stderr_Watch (Self : in out Channels);

   procedure Shutdown
     (Parent : in out Glib.IOChannel.Giochannel;
      Event  : in out Glib.Main.G_Source_Id;
      Lock   : in out Glib.Gboolean);
   --  Common code to stop watching and to shutdown IO channel.

   procedure Start_Watch
     (Parent    : Glib.IOChannel.Giochannel;
      Event     : in out Glib.Main.G_Source_Id;
      Lock      : in out Glib.Gboolean;
      Condition : Glib.IOChannel.GIOCondition;
      Callback  : access function
        (Source    : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data      : access Spawn.Internal.Process_Reference)
         return Glib.Gboolean;
      Data      : access Spawn.Internal.Process_Reference)
     with Convention => C;
   --  Common code to start (continue) watching of the IO channel.

   procedure On_Close_Channels (Self : Channels);

   -----------------------------
   -- Close_Child_Descriptors --
   -----------------------------

   procedure Close_Child_Descriptors (Self : in out Channels) is
      Ignore : Interfaces.C.int;

   begin
      if Self.Stdin_Child /= -1 then
         Ignore := Spawn.Posix.close (Interfaces.C.int (Self.Stdin_Child));
         Self.Stdin_Child := -1;
      end if;

      if Self.Stdout_Child /= -1 then
         Ignore := Spawn.Posix.close (Interfaces.C.int (Self.Stdout_Child));
         Self.Stdout_Child := -1;
      end if;

      if Self.Stderr_Child /= -1 then
         Ignore := Spawn.Posix.close (Interfaces.C.int (Self.Stderr_Child));
         Self.Stderr_Child := -1;
      end if;

      if Self.PTY_Slave /= -1 then
         Ignore := Spawn.Posix.close (Interfaces.C.int (Self.PTY_Slave));
         Self.PTY_Slave := -1;
      end if;
   end Close_Child_Descriptors;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Self : Channels) return Boolean is
   begin
      return Self.Stdout_Event /= Glib.Main.No_Source_Id
        or Self.Stderr_Event /= Glib.Main.No_Source_Id;
   end Is_Active;

   -----------------------
   -- On_Close_Channels --
   -----------------------

   procedure On_Close_Channels (Self : Channels) is
   begin
      if Self.Process.Pending_Finish then
         Self.Process.Pending_Finish := False;
         Self.Process.Status := Not_Running;

         Self.Process.Emit_Finished
           (Self.Process.Exit_Status, Self.Process.Exit_Code);
      end if;
   exception
      when others =>
         null;
   end On_Close_Channels;

   ---------------------
   -- On_Stderr_Event --
   ---------------------

   function On_Stderr_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      pragma Unreferenced (source);

      Self : Channels renames data.Self.Channels;

   begin
      if (condition and Glib.IOChannel.G_Io_In) /= 0 then
         Self.Stderr_Lock := @ - 1;

         Self.Process.Emit_Stderr_Available;

         if Self.Stderr_Lock = 0 then
            Self.Stderr_Event := Glib.Main.No_Source_Id;
         end if;
      end if;

      if (condition and Glib.IOChannel.G_Io_Hup) /= 0 then
         Self.Stderr_Lock := 0;
         Self.Stderr_Event := Glib.Main.No_Source_Id;

         if Self.Stdout_Event = Glib.Main.No_Source_Id then
            On_Close_Channels (Self);
         end if;
      end if;

      return Self.Stderr_Lock;
   end On_Stderr_Event;

   --------------------
   -- On_Stdin_Event --
   --------------------

   function On_Stdin_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      pragma Unreferenced (source);
      pragma Unreferenced (condition);

      Self : Channels renames data.Self.Channels;

   begin
      Self.Stdin_Lock := @ - 1;

      Self.Process.Emit_Stdin_Available;

      if Self.Stdin_Lock = 0 then
         Self.Stdin_Event := Glib.Main.No_Source_Id;
      end if;

      return Self.Stdin_Lock;
   end On_Stdin_Event;

   ---------------------
   -- On_Stdout_Event --
   ---------------------

   function On_Stdout_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      pragma Unreferenced (source);

      Self : Channels renames data.Self.Channels;

   begin
      if (condition and Glib.IOChannel.G_Io_In) /= 0 then
         Self.Stdout_Lock := @ - 1;

         Self.Process.Emit_Stdout_Available;

         if Self.Stdout_Lock = 0 then
            Self.Stdout_Event := Glib.Main.No_Source_Id;
         end if;
      end if;

      if (condition and Glib.IOChannel.G_Io_Hup) /= 0 then
         Self.Stdout_Lock := 0;
         Self.Stdout_Event := Glib.Main.No_Source_Id;

         if Self.Stderr_Event = Glib.Main.No_Source_Id then
            On_Close_Channels (Self);
         end if;
      end if;

      return Self.Stdout_Lock;
   end On_Stdout_Event;

   ---------------
   -- PTY_Slave --
   ---------------

   function PTY_Slave (Self : Channels) return Glib.Gint is
   begin
      return Self.PTY_Slave;
   end PTY_Slave;

   -----------------
   -- Read_Stderr --
   -----------------

   procedure Read_Stderr
     (Self    : in out Channels;
      Data    : out Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Success : in out Boolean)
   is
      use type Glib.Gsize;

      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : Glib.IOChannel.GIOStatus;

   begin
      Last := Data'First - 1;
      --  Mark operation as failed. On success of the opration corresponding
      --  value is set.

      if Self.Stderr_Parent = null then
         return;
      end if;

      Status :=
        Glib.IOChannel.Read_Chars
          (Self       => Self.Stderr_Parent,
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

      case Status is
         when Glib.IOChannel.G_Io_Status_Eof =>
            --  Reading is completed, so no watching is required

            Last := Data'First - 1;

         when Glib.IOChannel.G_Io_Status_Normal =>
            --  Read success, so no watching is required

            Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

         when Glib.IOChannel.G_Io_Status_Again =>
            --  No data to read, so start to watching again

            pragma Assert (Count = 0);
            Last := Data'First - 1;

            Start_Stderr_Watch (Self);

         when Glib.IOChannel.G_Io_Status_Error =>
            Success := False;
      end case;
   end Read_Stderr;

   -----------------
   -- Read_Stdout --
   -----------------

   procedure Read_Stdout
     (Self    : in out Channels;
      Data    : out Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Success : in out Boolean)
   is
      use type Glib.Gsize;

      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : Glib.IOChannel.GIOStatus;

   begin
      Last := Data'First - 1;
      --  Mark operation as failed. On success of the opration corresponding
      --  value is set.

      if Self.Stdout_Parent = null then
         return;
      end if;

      Status :=
        Glib.IOChannel.Read_Chars
          (Self       => Self.Stdout_Parent,
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

      case Status is
         when Glib.IOChannel.G_Io_Status_Eof =>
            --  Reading is completed, so no watching is required
            Last := Data'First - 1;

         when Glib.IOChannel.G_Io_Status_Normal =>
            --  Read success, so no watching is required
            Last :=
              Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

         when Glib.IOChannel.G_Io_Status_Again =>
            --  No data to read, so start to watching again
            pragma Assert (Count = 0);
            Last := Data'First - 1;

            Start_Stdout_Watch (Self);

         when Glib.IOChannel.G_Io_Status_Error =>
            Success := False;
      end case;
   end Read_Stdout;

   --------------------
   -- Setup_Channels --
   --------------------

   procedure Setup_Channels
     (Self     : in out Channels;
      Use_PTY  : Spawn.Common.Pipe_Flags;
      Child    : out Pipe_Array)
   is

      procedure Setup_Pipe
        (Kind    : Spawn.Posix.Pipe_Ends;
         Parent  : out Glib.IOChannel.Giochannel;
         Child   : out Glib.Gint;
         Success : in out Boolean);
      --  Create a pipe of two file descriptors. Wrap one of them (according
      --  to Kind) into a Giochannel and return as Parent. Return another as
      --  Child. If something goes wrong emit Error_Occurred in the process
      --  listener and change Success to False.

      procedure Setup_PTY (Success : in out Boolean);

      PTY_Channel : Glib.IOChannel.Giochannel := null;

      ----------------
      -- Setup_Pipe --
      ----------------

      procedure Setup_Pipe
        (Kind    : Spawn.Posix.Pipe_Ends;
         Parent  : out Glib.IOChannel.Giochannel;
         Child   : out Glib.Gint;
         Success : in out Boolean)
      is
         use type Glib.IOChannel.GIOStatus;

         procedure Cleanup;
         --  Close file descriptors and unreference channel.

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
            Ignore : Interfaces.C.int;

         begin
            Glib.IOChannel.Unref (Parent);
            Ignore := Spawn.Posix.close (Interfaces.C.int (Child));

            Parent  := null;
            Child := -1;
         end Cleanup;

         Fds   : Spawn.Posix.Fd_Pair;
         Error : aliased Glib.Error.GError;

         Opposite_End : constant array (Spawn.Posix.Pipe_Ends)
           of Spawn.Posix.Pipe_Ends :=
             [Spawn.Posix.Read_End  => Spawn.Posix.Write_End,
              Spawn.Posix.Write_End => Spawn.Posix.Read_End];
      begin
         Parent  := null;
         Child := -1;

         if not Success then
            return;
         end if;

         --  Create pipe

         if Spawn.Posix.pipe2 (Fds, Posix.O_CLOEXEC) /= 0 then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Create GIOChannel and move ownership of the file descriptor to
         --  the channel

         Parent := Glib.IOChannel.Giochannel_Unix_New (Glib.Gint (Fds (Kind)));
         Glib.IOChannel.Set_Close_On_Unref (Parent, True);

         Child := Glib.Gint (Fds (Opposite_End (Kind)));

         --  Setup non-blocking mode for the channel

         if Glib.IOChannel.Set_Flags
           (Parent,
            Glib.IOChannel.G_Io_Flag_Nonblock,
            Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Setup null encoding

         if Glib.IOChannel.Set_Encoding (Parent, "", Error'Access)
           /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Disable buffering.

         Glib.IOChannel.Set_Buffered (Parent, False);
      end Setup_Pipe;

      ---------------
      -- Setup_PTY --
      ---------------

      procedure Setup_PTY (Success : in out Boolean) is
         use type Glib.IOChannel.GIOStatus;

         procedure Cleanup;

         PTY_Master : Interfaces.C.int;
         Status     : Interfaces.C.int;
         Slave_Name : Interfaces.C.char_array (1 .. 64);
         Error      : aliased Glib.Error.GError;

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
            Ignore : Interfaces.C.int;

         begin
            if PTY_Master /= -1 then
               Ignore := Spawn.Posix.close (PTY_Master);
               PTY_Master := -1;
            end if;

            if Self.PTY_Slave /= -1 then
               Ignore :=
                 Spawn.Posix.close (Interfaces.C.int (Self.PTY_Slave));
               Self.PTY_Slave := -1;
            end if;

            if PTY_Channel /= null then
               Glib.IOChannel.Unref (PTY_Channel);
               PTY_Channel := null;
            end if;
         end Cleanup;

      begin
         PTY_Channel    := null;
         Self.PTY_Slave := -1;

         if not Success then
            return;
         end if;

         --  Open pseudoterminal's master descriptor

         PTY_Master :=
           Spawn.Posix.posix_openpt
             (Spawn.Posix.O_RDWR + Spawn.Posix.O_NOCTTY);

         if PTY_Master = -1 then
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Mark file descriptor as be closed on exec

         if Spawn.Posix.fcntl
           (PTY_Master, Spawn.Posix.F_SETFD, Spawn.Posix.FD_CLOEXEC)
           = -1
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

         Status :=
           Spawn.Posix.ptsname_r
             (PTY_Master, Slave_Name, Slave_Name'Length);

         if Status /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred (Integer (Status));
            Success := False;

            return;
         end if;

         --  Open slave device

         Self.PTY_Slave :=
           Glib.Gint
             (Spawn.Posix.open
                (Slave_Name, Spawn.Posix.O_RDWR + Spawn.Posix.O_CLOEXEC, 0));

         if Self.PTY_Slave = -1 then
            Cleanup;
            Self.Process.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
            Success := False;

            return;
         end if;

         --  Create GIOChannel and move ownership of the file descriptor to
         --  the channel

         PTY_Channel :=
           Glib.IOChannel.Giochannel_Unix_New (Glib.Gint (PTY_Master));
         Glib.IOChannel.Set_Close_On_Unref (PTY_Channel, True);
         PTY_Master := -1;

         --  Setup non-blocking mode for the channel

         if Glib.IOChannel.Set_Flags
           (PTY_Channel,
            Glib.IOChannel.G_Io_Flag_Nonblock,
            Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Setup null encoding

         if Glib.IOChannel.Set_Encoding (PTY_Channel, "", Error'Access)
           /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Disable buffering.

         Glib.IOChannel.Set_Buffered (PTY_Channel, False);
      end Setup_PTY;

      Success : Boolean := True;

      use all type Spawn.Common.Pipe_Kinds;
      use type Spawn.Common.Pipe_Flags;
   begin
      if Use_PTY /= [Use_PTY'Range => False] then
         Setup_PTY (Success);
         Child := [Child'Range => Self.PTY_Slave];
      end if;

      if Use_PTY (Stdin) then
         Self.Stdin_Parent := Glib.IOChannel.Ref (PTY_Channel);
      else
         Setup_Pipe
           (Spawn.Posix.Write_End,
            Self.Stdin_Parent,
            Self.Stdin_Child,
            Success);

         Child (Stdin) := Self.Stdin_Child;
      end if;

      if Use_PTY (Stdout) then
         Self.Stdout_Parent := Glib.IOChannel.Ref (PTY_Channel);
      else
         Setup_Pipe
           (Spawn.Posix.Read_End,
            Self.Stdout_Parent,
            Self.Stdout_Child,
            Success);

         Child (Stdout) := Self.Stdout_Child;
      end if;

      if Use_PTY (Stderr) then
         Self.Stderr_Parent := Glib.IOChannel.Ref (PTY_Channel);
      else
         Setup_Pipe
           (Spawn.Posix.Read_End,
            Self.Stderr_Parent,
            Self.Stderr_Child,
            Success);

         Child (Stderr) := Self.Stderr_Child;
      end if;

      if PTY_Channel /= null then
         Glib.IOChannel.Unref (PTY_Channel);
      end if;
   end Setup_Channels;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Parent : in out Glib.IOChannel.Giochannel;
      Event  : in out Glib.Main.G_Source_Id;
      Lock   : in out Glib.Gboolean) is
   begin
      if Event /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Event);
         Event := Glib.Main.No_Source_Id;
         Lock := 0;
      end if;

      if Parent /= null then
         Glib.IOChannel.Unref (Parent);
         Parent := null;
      end if;
   end Shutdown;

   -----------------------
   -- Shutdown_Channels --
   -----------------------

   procedure Shutdown_Channels (Self : in out Channels) is
   begin
      Close_Child_Descriptors (Self);

      Shutdown_Stdin (Self);
      Shutdown_Stdout (Self);
      Shutdown_Stderr (Self);
   end Shutdown_Channels;

   ---------------------
   -- Shutdown_Stderr --
   ---------------------

   procedure Shutdown_Stderr (Self : in out Channels) is
   begin
      Shutdown (Self.Stderr_Parent, Self.Stderr_Event, Self.Stderr_Lock);
   end Shutdown_Stderr;

   --------------------
   -- Shutdown_Stdin --
   --------------------

   procedure Shutdown_Stdin (Self : in out Channels) is
   begin
      Shutdown (Self.Stdin_Parent, Self.Stdin_Event, Self.Stdin_Lock);
   end Shutdown_Stdin;

   ---------------------
   -- Shutdown_Stdout --
   ---------------------

   procedure Shutdown_Stdout (Self : in out Channels) is
   begin
      Shutdown (Self.Stdout_Parent, Self.Stdout_Event, Self.Stdout_Lock);
   end Shutdown_Stdout;

   ------------------------
   -- Start_Stderr_Watch --
   ------------------------

   procedure Start_Stderr_Watch (Self : in out Channels) is
   begin
      if Self.Stderr_Parent /= Self.Stdout_Parent then
         Start_Watch
           (Self.Stderr_Parent,
            Self.Stderr_Event,
            Self.Stderr_Lock,
            Glib.IOChannel.G_Io_In + Glib.IOChannel.G_Io_Hup,
            On_Stderr_Event'Access,
            Self.Process.Reference'Unchecked_Access);
      end if;
   end Start_Stderr_Watch;

   -----------------------
   -- Start_Stdin_Watch --
   -----------------------

   procedure Start_Stdin_Watch (Self : in out Channels) is
   begin
      Start_Watch
        (Self.Stdin_Parent,
         Self.Stdin_Event,
         Self.Stdin_Lock,
         Glib.IOChannel.G_Io_Out,
         On_Stdin_Event'Access,
         Self.Process.Reference'Unchecked_Access);
   end Start_Stdin_Watch;

   ------------------------
   -- Start_Stdout_Watch --
   ------------------------

   procedure Start_Stdout_Watch (Self : in out Channels) is
   begin
      Start_Watch
        (Self.Stdout_Parent,
         Self.Stdout_Event,
         Self.Stdout_Lock,
         Glib.IOChannel.G_Io_In + Glib.IOChannel.G_Io_Hup,
         On_Stdout_Event'Access,
         Self.Process.Reference'Unchecked_Access);
   end Start_Stdout_Watch;

   -----------------
   -- Start_Watch --
   -----------------

   procedure Start_Watch
     (Parent    : Glib.IOChannel.Giochannel;
      Event     : in out Glib.Main.G_Source_Id;
      Lock      : in out Glib.Gboolean;
      Condition : Glib.IOChannel.GIOCondition;
      Callback  : access function
        (Source    : Glib.IOChannel.Giochannel;
         Condition : Glib.IOChannel.GIOCondition;
         Data      : access Spawn.Internal.Process_Reference)
         return Glib.Gboolean;
      Data      : access Spawn.Internal.Process_Reference) is
   begin
      if Event = Glib.Main.No_Source_Id then
         pragma Assert (Lock = 0);

         Event := Add_Watch (Parent, Condition, Callback, Data);
         Lock := 1;

      else
         Lock := @ + 1;
      end if;
   end Start_Watch;

   -----------------
   -- Start_Watch --
   -----------------

   procedure Start_Watch (Self : in out Channels) is
   begin
      Start_Stdin_Watch (Self);
      Start_Stdout_Watch (Self);
      Start_Stderr_Watch (Self);
   end Start_Watch;

   -----------------
   -- Write_Stdin --
   -----------------

   procedure Write_Stdin
     (Self    : in out Channels;
      Data    : Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Success : in out Boolean)
   is
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize;
      Status : Glib.IOChannel.GIOStatus;

   begin
      Last := Data'First - 1;
      --  Mark operation as failed. On success of the opration corresponding
      --  value is set.

      if Self.Stdout_Parent = null then
         return;
      end if;

      Status :=
        Glib.IOChannel.Write_Chars
          (Self          => Self.Stdin_Parent,
           Buf           => Data,
           Bytes_Written => Count'Access,
           Error         => Error'Access);
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

      case Status is
         when Glib.IOChannel.G_Io_Status_Normal =>
            null;

         when Glib.IOChannel.G_Io_Status_Again =>
            --  There is no enough space in the buffer to write, so start
            --  watching again

            Start_Stdin_Watch (Self);

         when Glib.IOChannel.G_Io_Status_Error =>
            Success := False;

         when others =>
            raise Program_Error;
      end case;
   end Write_Stdin;

end Spawn.Channels;
