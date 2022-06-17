--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;

pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with Glib.Error;
with Glib.Main;
with Glib.Spawn;
with Gtkada.Types;

with Spawn.Channels;
with Spawn.Environments.Internal;
with Spawn.Posix;

separate (Spawn.Processes)
package body Platform is

   procedure Do_Start_Process (Self : aliased in out Process'Class);

   function Child_Watch is new Glib.Main.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Callback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   type Process_Access is access all Process'Class;

   function Spawn_Async_With_Fds is
     new Glib.Spawn.Generic_Spawn_Async_With_Fds
       (User_Data => Glib.Gint);

   procedure Setup_Child_Process (Fd : access Glib.Gint)
     with Convention => C;
   --  Setup session and controlling terminal when pseudoterminal is used
   --  for interprocess communication.

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Spawn.Channels.Shutdown_Stderr (Self.Channels);
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Spawn.Channels.Shutdown_Stdin (Self.Channels);
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Spawn.Channels.Shutdown_Stdout (Self.Channels);
   end Close_Standard_Output;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      use Ada.Strings.Unbounded;
      use Glib;
      use type Interfaces.C.size_t;

      procedure Prepare_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array);
      --  Allocate argumnets

      -----------------------
      -- Prepare_Arguments --
      -----------------------

      procedure Prepare_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         argv (0) := Gtkada.Types.New_String
           (To_String (Self.Program));

         for J in 1 .. Self.Arguments.Last_Index loop
            argv (Interfaces.C.size_t (J)) := Gtkada.Types.New_String
              (Self.Arguments.Element (J));
         end loop;

         argv (argv'Last) := Gtkada.Types.Null_Ptr;
      end Prepare_Arguments;

      dir  : Gtkada.Types.Chars_Ptr :=
        (if Length (Self.Directory) = 0 then Gtkada.Types.Null_Ptr
           else Gtkada.Types.New_String
             (To_String (Self.Directory)));

      argv : aliased Gtkada.Types.Chars_Ptr_Array :=
        (0 .. Interfaces.C.size_t (Self.Arguments.Length) + 1 => <>);

      envp : aliased Gtkada.Types.Chars_Ptr_Array :=
        Spawn.Environments.Internal.Raw (Self.Environment);

      Error  : aliased Glib.Error.GError;
      Status : Glib.Gboolean;
      PTY    : aliased Glib.Gint;

   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Prepare_Arguments (argv);
      Spawn.Channels.Setup_Channels
        (Self.Channels,
         Self.Use_PTY (Stdin),
         Self.Use_PTY (Stdout),
         Self.Use_PTY (Stderr));

      PTY := Spawn.Channels.PTY_Slave (Self.Channels);
      Status :=
        Spawn_Async_With_Fds
          (Working_Directory => dir,
           Argv              => argv'Access,
           Envp              => envp'Access,
           Flags             => Glib.Spawn.G_Spawn_Do_Not_Reap_Child,
           Child_Setup       => Setup_Child_Process'Access,
           Data              => PTY'Unchecked_Access,
           Child_Pid         => Self.pid'Access,
           Stdin_Fd          => Spawn.Channels.Child_Stdin (Self.Channels),
           Stdout_Fd         => Spawn.Channels.Child_Stdout (Self.Channels),
           Stderr_Fd         => Spawn.Channels.Child_Stderr (Self.Channels),
           Error             => Error'Access);

      Gtkada.Types.Free (argv);
      Gtkada.Types.Free (envp);
      Gtkada.Types.Free (dir);
      Spawn.Channels.Close_Child_Descriptors (Self.Channels);

      if Status = 0 then
         Spawn.Channels.Shutdown_Channels (Self.Channels);

         Self.Listener.Error_Occurred (Integer (Glib.Error.Get_Code (Error)));

         return;
      end if;

      Self.Event := Child_Watch
        (Self.pid,
         My_Death_Callback'Access,
         Self.Reference'Access);

      Self.Status := Running;
      Self.Listener.Started;

      Spawn.Channels.Start_Watch (Self.Channels);
   end Do_Start_Process;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Process'Class) is
      use type Glib.Main.G_Source_Id;

   begin
      Spawn.Channels.Shutdown_Channels (Self.Channels);

      if Self.Event /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Self.Event);
         Self.Event := Glib.Main.No_Source_Id;
      end if;

      Glib.Spawn.Spawn_Close_Pid (Self.pid);
      Self.pid := 0;
   end Finalize;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
      use type Interfaces.C.int;

      Code : constant Interfaces.C.int := Spawn.Posix.kill
        (Interfaces.C.int (Self.pid),
         Interfaces.C.int (System.OS_Interface.SIGKILL));
   begin
      pragma Assert (Code = 0);
   end Kill_Process;

   -----------------------
   -- My_Death_Callback --
   -----------------------

   procedure My_Death_Callback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
   is
      use type Glib.GQuark;

      function G_SPAWN_EXIT_ERROR return Glib.GQuark
        with Import        => True,
             Convention    => C,
             External_Name => "g_spawn_exit_error_quark";

      Self  : constant Process_Access := Process_Access (data.Self);
      Error : aliased Glib.Error.GError;

   begin
      Glib.Spawn.Spawn_Close_Pid (pid);

      Self.Event  := Glib.Main.No_Source_Id;
      Self.Status := Not_Running;
      Self.pid    := 0;

      if Glib.Spawn.Spawn_Check_Exit_Status
        (status, Error'Access)
      then
         Self.Exit_Status := Normal;
         Self.Exit_Code   := 0;

      else
         Self.Exit_Status :=
           (if Glib.Error.Get_Domain (Error) = G_SPAWN_EXIT_ERROR
              then Normal
              else Crash);
         Self.Exit_Code   := Process_Exit_Code (Glib.Error.Get_Code (Error));
      end if;

      begin
         Self.Listener.Finished (Self.Exit_Status, Self.Exit_Code);

      exception
         when others =>
            null;
      end;

      Spawn.Channels.Shutdown_Channels (Self.Channels);

   exception
      when E : others =>
         Self.Listener.Exception_Occurred (E);
   end My_Death_Callback;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Spawn.Channels.Read_Stderr (Self.Channels, Data, Last);
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Spawn.Channels.Read_Stdout (Self.Channels, Data, Last);
   end Read_Standard_Output;

   -------------------------
   -- Setup_Child_Process --
   -------------------------

   procedure Setup_Child_Process (Fd : access Glib.Gint) is
      use type Glib.Gint;

      Ignore : Interfaces.C.int;
      --  Any failures are ignored.

   begin
      if Fd.all /= -1 then
         Ignore := Spawn.Posix.setsid;
         Ignore :=
           Spawn.Posix.ioctl
             (Interfaces.C.int (Fd.all), Spawn.Posix.TIOCSCTTY, 0);
      end if;
   end Setup_Child_Process;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Status := Starting;
      Self.Exit_Code := -1;
      Do_Start_Process (Self);
   end Start;

   -----------------------
   -- Terminate_Process --
   -----------------------

   procedure Terminate_Process (Self : in out Process'Class) is
      use type Interfaces.C.int;

      Code : constant Interfaces.C.int := Spawn.Posix.kill
        (Interfaces.C.int (Self.pid),
         Interfaces.C.int (System.OS_Interface.SIGTERM));
   begin
      pragma Assert (Code = 0);
   end Terminate_Process;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Spawn.Channels.Write_Stdin (Self.Channels, Data, Last);
   end Write_Standard_Input;

end Platform;
