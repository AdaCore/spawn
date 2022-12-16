--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Interrupts.Names;
with Ada.Strings.Unbounded;

with Interfaces.C.Strings;

with GNAT.OS_Lib;

with Spawn.Environments.Internal;
with Spawn.Posix;
with Spawn.Polls.POSIX_Polls;

package body Spawn.Internal.Monitor is
   use type Interfaces.C.int;
   use all type Pipe_Kinds;

   type Process_Access is access all Process'Class;

   procedure Start_Process (Self : Process_Access);

   procedure Do_Close_Pipe
     (Self : Process_Access;
      Kind : Common.Standard_Pipe);

   procedure Check_Children;

   package Command_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (Command);

   package Command_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Command_Queue_Interfaces);

   Queue : Command_Queues.Queue;
   Poll  : Spawn.Polls.Poll_Access;
   Wake  : Interfaces.C.int := -1;

   type Wake_Up_Listener is new Spawn.Polls.Listener with null record;

   overriding procedure On_Event
     (Self   : in out Wake_Up_Listener;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set);
   --  Restart watching of the pipe descriptor.

   function Hash (Value : Interfaces.C.int) return Ada.Containers.Hash_Type;

   package Process_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Interfaces.C.int,
      Element_Type    => Process_Access,
      Hash            => Hash,
      Equivalent_Keys => Interfaces.C."=",
      "="             => "=");

   Map : Process_Maps.Map;

   Pipe_Flags : constant Interfaces.C.int := Posix.O_CLOEXEC;

   To_Event_Set : constant array (Spawn.Common.Pipe_Kinds) of
     Spawn.Polls.Watch_Event_Set :=
       (Spawn.Common.Stdin => Spawn.Polls.Output,
        others             => Spawn.Polls.Input);

   protected SIGCHLD is
      entry Wait;

      procedure Handle
        with Interrupt_Handler,
             Attach_Handler => Ada.Interrupts.Names.SIGCHLD;
   private
      Fired : Boolean := False;
   end SIGCHLD;

   protected body SIGCHLD is

      entry Wait when Fired is
      begin
         Fired := False;
      end Wait;

      procedure Handle is
      begin
         Fired := True;
      end Handle;

   end SIGCHLD;

   --------------------
   -- Check_Children --
   --------------------

   procedure Check_Children is

      function WIFEXITED (Status : Interfaces.C.unsigned) return Boolean;

      function WEXITSTATUS
        (Status : Interfaces.C.unsigned) return Interfaces.C.unsigned
           with Import        => True,
                Convention    => C,
                External_Name => "__spawn_WEXITSTATUS";

      function WIFSIGNALED (Status : Interfaces.C.unsigned) return Boolean;

      function WTERMSIG
        (Status : Interfaces.C.unsigned) return Interfaces.C.unsigned
           with Import, Convention => C, External_Name => "__spawn_WTERMSIG";

      ---------------
      -- WIFEXITED --
      ---------------

      function WIFEXITED (Status : Interfaces.C.unsigned) return Boolean is
         function Imported
           (Status : Interfaces.C.unsigned) return Interfaces.C.int
              with Import        => True,
                   Convention    => C,
                   External_Name => "__spawn_WIFEXITED";

      begin
         return Imported (Status) /= 0;
      end WIFEXITED;

      -----------------
      -- WIFSIGNALED --
      -----------------

      function WIFSIGNALED (Status : Interfaces.C.unsigned) return Boolean is
         function Imported
           (Status : Interfaces.C.unsigned) return Interfaces.C.int
              with Import        => True,
                   Convention    => C,
                   External_Name => "__spawn_WIFSIGNALED";

      begin
         return Imported (Status) /= 0;
      end WIFSIGNALED;

      status : aliased Interfaces.C.unsigned := 0;
      pid    : constant Interfaces.C.int :=
        Posix.waitpid (-1, status'Unchecked_Access, Posix.WNOHANG);

      Cursor  : constant Process_Maps.Cursor := Map.Find (pid);
      Process : Process_Access;

   begin
      if Process_Maps.Has_Element (Cursor) then
         Process := Process_Maps.Element (Cursor);

         Process.Exit_Status := (if WIFEXITED (status) then Normal else Crash);

         case Process.Exit_Status is
            when Normal =>
               Process.Exit_Code := Process_Exit_Code (WEXITSTATUS (status));

            when Crash =>
               Process.Exit_Code :=
                 (if WIFSIGNALED (status)
                  then Process_Exit_Code (WTERMSIG (status))
                  else Process_Exit_Code'Last);
         end case;

         Process.Status := Not_Running;

         Process.Emit_Finished (Process.Exit_Status, Process.Exit_Code);
      end if;
   end Check_Children;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : Process_Access;
      Kind : Common.Standard_Pipe)
   is
      Ignore : Interfaces.C.int;
   begin
      Poll.Watch (Self.pipe (Kind), Spawn.Polls.Empty_Set, null);
      Ignore := Posix.close (Self.pipe (Kind));
   end Do_Close_Pipe;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Value : Command) is
      Ignore : Interfaces.C.size_t;
   begin
      Queue.Enqueue (Value);
      --  Wake up monitoring tread.
      Ignore := Posix.write (Wake, (1 => 0), 1);
   end Enqueue;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Interfaces.C.int) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (abs Value);
   end Hash;

   ----------------
   -- Loop_Cycle --
   ----------------

   procedure Loop_Cycle (Timeout : Duration) is
      use type Ada.Containers.Count_Type;

      Command : Monitor.Command;
   begin
      select
         SIGCHLD.Wait;
         Check_Children;
      else
         null;
      end select;

      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Command);

         case Command.Kind is
            when Start =>
               Start_Process (Process_Access (Command.Process));
            when Close_Pipe =>
               Do_Close_Pipe (Command.Process, Command.Pipe);
            when Watch_Pipe =>
               Poll.Watch
                 (Value    => Command.Process.pipe (Command.Pipe),
                  Events   => To_Event_Set (Command.Pipe),
                  Listener => Polls.Listener_Access (Command.Process));
         end case;
      end loop;

      Poll.Wait (Timeout);
   end Loop_Cycle;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process (Self : Process_Access) is
      use Ada.Strings.Unbounded;
      use type Interfaces.C.Strings.chars_ptr;
      use type Ada.Streams.Stream_Element_Offset;

      use all type Posix.Pipe_Ends;

      procedure Send_Errno with No_Return;
      --  Put errno into Launch pipe end abort process
      procedure Prepare_Arguments (argv : out Posix.chars_ptr_array);
      --  Allocate argumnets
      procedure Free (argv : out Posix.chars_ptr_array);
      --  Deallocate argumnets

      --------------------
      -- Free_Arguments --
      --------------------

      procedure Free (argv : out Posix.chars_ptr_array) is
      begin
         for J in argv'Range loop
            Interfaces.C.Strings.Free (argv (J));
         end loop;
      end Free;

      -----------------------
      -- Prepare_Arguments --
      -----------------------

      procedure Prepare_Arguments (argv : out Posix.chars_ptr_array) is
      begin
         argv (0) := Interfaces.C.Strings.New_String (Self.Program);

         for J in 1 .. Self.Arguments.Last_Index loop
            argv (J) := Interfaces.C.Strings.New_String
              (Self.Arguments.Element (J));
         end loop;

         argv (argv'Last) := Interfaces.C.Strings.Null_Ptr;
      end Prepare_Arguments;

      std : array (Pipe_Kinds) of Posix.Fd_Pair;
      Child_Ends : constant array (std'Range) of Posix.Pipe_Ends :=
        (Stdin => Posix.Read_End, others => Posix.Write_End);
      Parent_Ends : constant array (std'Range) of Posix.Pipe_Ends :=
        (Stdin => Posix.Write_End, others => Posix.Read_End);
      Dup : constant array (Stdin .. Stderr) of Interfaces.C.int := (0, 1, 2);
      r : Interfaces.C.int;
      pragma Unreferenced (r);

      ----------------
      -- Send_Errno --
      ----------------

      procedure Send_Errno is
         count : Interfaces.C.size_t;
         pragma Unreferenced (count);
         errno : Integer;
         Error_Dump : Ada.Streams.Stream_Element_Array (1 .. errno'Size / 8)
           with Import, Convention => Ada, Address => errno'Address;
      begin
         errno := GNAT.OS_Lib.Errno;
         count := Posix.write
           (std (Launch) (Child_Ends (Launch)),
            Error_Dump,
            Error_Dump'Length);
         GNAT.OS_Lib.OS_Exit (127);
      end Send_Errno;

      pid  : Interfaces.C.int;
      dir  : Interfaces.C.Strings.chars_ptr :=
        (if Length (Self.Directory) = 0 then Interfaces.C.Strings.Null_Ptr
           else Interfaces.C.Strings.New_String
             (To_String (Self.Directory)));

      argv : Posix.chars_ptr_array (0 .. Natural (Self.Arguments.Length) + 1);
      envp : Posix.chars_ptr_array :=
        Spawn.Environments.Internal.Raw (Self.Environment);
   begin
      --  Create pipes for children's strio
      if (for some X of std => Posix.pipe2 (X, Pipe_Flags) /= 0) then
         Self.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
         Interfaces.C.Strings.Free (dir);
         return;
      end if;

      Prepare_Arguments (argv);

      pid := Posix.fork;

      if pid = -1 then
         --  Fork failed
         Self.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
         Free (argv);
         Free (envp);
         Interfaces.C.Strings.Free (dir);
         return;
      elsif pid = 0 then  --  Child process
         --  Close unused ends
         if (for some X in std'Range =>
               Posix.close (std (X) (Parent_Ends (X))) /= 0)
         then
            Send_Errno;
         --  Copy fd to standard numbers
         elsif (for some X in Dup'Range =>
                  Posix.dup2 (std (X) (Child_Ends (X)), Dup (X)) = -1)
         then
            Send_Errno;
         --  Change directory if needed
         elsif dir /= Interfaces.C.Strings.Null_Ptr
           and then Posix.chdir (dir) /= 0
         then
            Send_Errno;
         else  --  Replace executable
            r := Posix.execve (argv (0), argv, envp);
            Send_Errno;
         end if;
      end if;

      --  Parent process
      Free (argv);
      Free (envp);
      Interfaces.C.Strings.Free (dir);

      --  Close unused ends
      if (for some X in std'Range =>
            Posix.close (std (X) (Child_Ends (X))) /= 0)
      then
         Self.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
         return;
      end if;

      --  Make stdio non-blocking
      if (for some X in Common.Standard_Pipe =>
            Posix.fcntl
              (std (X) (Parent_Ends (X)),
               Posix.F_SETFL,
               Posix.O_NONBLOCK) /= 0)
      then
         Self.Emit_Error_Occurred (GNAT.OS_Lib.Errno);
         return;
      end if;

      Self.pid := pid;
      Map.Insert (pid, Self);

      for X in Self.pipe'Range loop
         Self.pipe (X) := std (X) (Parent_Ends (X));
         if X /= Stdin then
            Poll.Watch
              (Value    => Self.pipe (X),
               Events   => To_Event_Set (X),
               Listener => Spawn.Polls.Listener_Access (Self));
         end if;
      end loop;
   end Start_Process;

   procedure Initialize;
   --  Do low level initialization if needed

   procedure Dummy is null;
   --  This is to be used in Initialize procedure

   procedure Initialize is separate;

   type POSIX_Poll_Access is access Polls.POSIX_Polls.POSIX_Poll;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Wake_Up_Listener;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set)
   is
      Byte   : Ada.Streams.Stream_Element_Array (1 .. 1);
      Ignore : Interfaces.C.size_t;
   begin
      if Events (Spawn.Polls.Input) then
         Ignore := Posix.read (Value, Byte, Byte'Length);
         Poll.Watch
           (Value    => Value,
            Events   => Spawn.Polls.Input,
            Listener => Self'Unchecked_Access);
      end if;
   end On_Event;

   WL : aliased Wake_Up_Listener;

begin
   declare
      Object : constant POSIX_Poll_Access := new Polls.POSIX_Polls.POSIX_Poll;
      Value  : Posix.Fd_Pair;
      Result : constant Interfaces.C.int := Posix.pipe2 (Value, Pipe_Flags);
   begin
      pragma Assert (Result = 0, GNAT.OS_Lib.Errno_Message);
      Wake := Value (Posix.Write_End);
      Poll := Spawn.Polls.Poll_Access (Object);
      Poll.Initialize;
      Poll.Watch
        (Value    => Value (Posix.Read_End),
         Events   => Spawn.Polls.Input,
         Listener => WL'Access);
   end;
   Initialize;
end Spawn.Internal.Monitor;
