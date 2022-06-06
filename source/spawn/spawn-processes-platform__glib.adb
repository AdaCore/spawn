--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;

pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with Spawn.Environments.Internal;
with Spawn.Posix;

with Glib.Error;
with Glib.IOChannel;
with Glib.Main;
with Glib.Spawn;
with Gtkada.Types;

separate (Spawn.Processes)
package body Platform is

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Standard_Pipe);

   procedure Do_Start_Process (Self : aliased in out Process'Class);

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe);

   function IO_Watch is new Glib.IOChannel.Generic_Add_Watch
     (User_Data => Internal.Process_Reference);

   function Child_Watch is new Glib.Main.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Callback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   function My_IO_Callback
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
        with Convention => C;

   type Process_Access is access all Process'Class;

   Map : constant array (Standard_Pipe) of Glib.IOChannel.GIOCondition :=
     (Stdin  => Glib.IOChannel.G_Io_Out,
      Stdout => Glib.IOChannel.G_Io_In,
      Stderr => Glib.IOChannel.G_Io_In);

   function Spawn_Async_With_Pipes is
     new Glib.Spawn.Generic_Spawn_Async_With_Pipes
       (User_Data => Integer);

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Do_Close_Pipe (Self, Stderr);
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Do_Close_Pipe (Self, Stdin);
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Do_Close_Pipe (Self, Stdout);
   end Close_Standard_Output;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Standard_Pipe)
   is
      use type Glib.IOChannel.Giochannel;
      use type Glib.IOChannel.GIOStatus;
      use type Glib.Main.G_Source_Id;

      Pipe  : Internal.Pipe_Record renames Self.pipe (Kind);
      Error : aliased Glib.Error.GError;

   begin
      if Pipe.Channel /= null then
         if Glib.IOChannel.Shutdown (Pipe.Channel, 1, Error'Access)
           = Glib.IOChannel.G_Io_Status_Normal
         then
            Glib.IOChannel.Unref (Pipe.Channel);
            Pipe.Channel := null;

            if Pipe.Event /= Glib.Main.No_Source_Id then
               Glib.Main.Remove (Pipe.Event);
               Pipe.Event := Glib.Main.No_Source_Id;
            end if;

         else
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
         end if;
      end if;
   end Do_Close_Pipe;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use Glib;
      use Glib.Main;

      Pipe   : Internal.Pipe_Record renames Self.pipe (Kind);
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Read_Chars
          (Self       => Pipe.Channel,
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event /= Glib.Main.No_Source_Id;
   begin
      --  Protect against uninitialized value
      Last := Data'First - 1;

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

            if In_Callback then
               --  Ask IO_Callback to continue watching
               Pipe.Watch := True;
            else
               --  Start watching here
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (Kind),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;
         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
      end case;
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      use Ada.Strings.Unbounded;
      use Glib;
      use type Interfaces.C.size_t;
      use type Glib.IOChannel.GIOStatus;

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

      Error : aliased Glib.Error.GError;
   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Prepare_Arguments (argv);

      if Spawn_Async_With_Pipes
        (Working_Directory => dir,
         Argv              => argv'Access,
         Envp              => envp'Access,
         Flags             => Glib.Spawn.G_Spawn_Do_Not_Reap_Child,
         Child_Setup       => null,
         Data              => null,
         Child_Pid         => Self.pid'Access,
         Standard_Input    => Self.pipe (Stdin).FD'Access,
         Standard_Output   => Self.pipe (Stdout).FD'Access,
         Standard_Error    => Self.pipe (Stderr).FD'Access,
         Error             => Error'Access) = 0
      then
         Self.Listener.Error_Occurred (Integer (Glib.Error.Get_Code (Error)));
         return;
      end if;

      Gtkada.Types.Free (argv);
      Gtkada.Types.Free (envp);
      Gtkada.Types.Free (dir);

      --  Create IO Channels and make them non-blocking
      for J in Standard_Pipe loop
         declare
            Pipe : Spawn.Internal.Pipe_Record renames Self.pipe (J);
         begin
            Pipe.Channel := Glib.IOChannel.Giochannel_Unix_New (Pipe.FD);

            if Glib.IOChannel.Set_Flags
              (Pipe.Channel,
               Glib.IOChannel.G_Io_Flag_Nonblock,
               Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            elsif Glib.IOChannel.Set_Encoding
              (Pipe.Channel,
               "",
               Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            end if;

            Glib.IOChannel.Set_Buffered (Pipe.Channel, False);

            if J /= Stdin then
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (J),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;
         end;
      end loop;

      Self.Event := Child_Watch
        (Self.pid,
         My_Death_Callback'Access,
         Self.Reference'Access);

      Self.Status := Running;
      Self.Listener.Started;
      Self.Listener.Standard_Input_Available;
   end Do_Start_Process;

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

      Process : constant Process_Access := Process_Access (data.Self);
      Error   : aliased Glib.Error.GError;
   begin
      for J in Standard_Pipe loop
         Do_Close_Pipe (Process.all, J);
      end loop;

      Glib.Spawn.Spawn_Close_Pid (pid);

      if Glib.Spawn.Spawn_Check_Exit_Status
        (status, Error'Access)
      then
         Process.Exit_Status := Normal;
         Process.Exit_Code   := 0;

      else
         Process.Exit_Status :=
           (if Glib.Error.Get_Domain (Error) = G_SPAWN_EXIT_ERROR
              then Normal
              else Crash);
         Process.Exit_Code := Process_Exit_Code (Glib.Error.Get_Code (Error));
      end if;

      Process.Status := Not_Running;
      Process.Listener.Finished (Process.Exit_Status, Process.Exit_Code);

   exception
      when E : others =>
         Process.Listener.Exception_Occurred (E);
   end My_Death_Callback;

   --------------------
   -- My_IO_Callback --
   --------------------

   function My_IO_Callback
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      use type Glib.IOChannel.Giochannel;
      use Glib.Main;

      Process : constant Process_Access := Process_Access (data.Self);
      Watch   : Glib.Gboolean := 0;
   begin
      case condition is
         when Glib.IOChannel.G_Io_In =>
            if Process.pipe (Stdout).Channel = source then
               pragma Assert
                 (Process.pipe (Stdout).Event /= Glib.Main.No_Source_Id);

               Process.pipe (Stdout).Watch := False;
               Process.Listener.Standard_Output_Available;

               if Process.pipe (Stdout).Watch then
                  Watch := 1;
               else
                  Process.pipe (Stdout).Event := Glib.Main.No_Source_Id;
                  Watch := 0;
               end if;
            else
               pragma Assert
                 (Process.pipe (Stderr).Event /= Glib.Main.No_Source_Id);

               Process.pipe (Stderr).Watch := False;
               Process.Listener.Standard_Error_Available;

               if Process.pipe (Stderr).Watch then
                  Watch := 1;
               else
                  Process.pipe (Stderr).Event := Glib.Main.No_Source_Id;
                  Watch := 0;
               end if;
            end if;
         when Glib.IOChannel.G_Io_Out =>
            pragma Assert
              (Process.pipe (Stdin).Event /= Glib.Main.No_Source_Id);

            Process.pipe (Stdin).Watch := False;
            Process.Listener.Standard_Input_Available;

            if Process.pipe (Stdin).Watch then
               Watch := 1;
            else
               Process.pipe (Stdin).Event := Glib.Main.No_Source_Id;
               Watch := 0;
            end if;
         when others =>
            raise Program_Error;
      end case;

      return Watch;
   exception
      when E : others =>
         Process.Listener.Exception_Occurred (E);
         return Watch;
   end My_IO_Callback;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Do_Read (Self, Data, Last, Stderr);
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Do_Read (Self, Data, Last, Stdout);
   end Read_Standard_Output;

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
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use Glib;
      use Glib.Main;
      use type Ada.Streams.Stream_Element_Offset;

      Pipe   : Internal.Pipe_Record renames Self.pipe (Stdin);
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize;

      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Write_Chars
          (Self          => Pipe.Channel,
           Buf           => Data,
           Bytes_Written => Count'Access,
           Error         => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event /= Glib.Main.No_Source_Id;

   begin
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

      case Status is
         when Glib.IOChannel.G_Io_Status_Normal =>
            null;

         when Glib.IOChannel.G_Io_Status_Again =>
            --  There is no enough space in the buffer to write, so start
            --  watching again

            if In_Callback then
               --  Ask IO_Callback to continue watching
               Pipe.Watch := True;

            else
               --  Start watching here
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (Stdin),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;

         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));

         when others =>
            raise Program_Error;
      end case;
   end Write_Standard_Input;

end Platform;
