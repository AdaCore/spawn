--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Interfaces.C;
pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with Glib.Error;

with Spawn.Internal;
with Spawn.Posix;

package body Spawn.Channels is

   use type Ada.Streams.Stream_Element_Offset;
   use type Glib.Gboolean;
   use type Glib.IOChannel.Giochannel;
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

   ------------------
   -- Child_Stderr --
   ------------------

   function Child_Stderr (Self : Channels) return Glib.Gint is
   begin
      return Self.Stderr_Child;
   end Child_Stderr;

   -----------------
   -- Child_Stdin --
   -----------------

   function Child_Stdin (Self : Channels) return Glib.Gint is
   begin
      return
        (if Self.Stdin_Child /= -1
           then Self.Stdin_Child else Self.PTY_Slave);
   end Child_Stdin;

   ------------------
   -- Child_Stdout --
   ------------------

   function Child_Stdout (Self : Channels) return Glib.Gint is
   begin
      return
        (if Self.Stdout_Child /= -1
           then Self.Stdout_Child else Self.PTY_Slave);
   end Child_Stdout;

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

   ---------------------
   -- On_Stderr_Event --
   ---------------------

   function On_Stderr_Event
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      pragma Unreferenced (source);
      pragma Unreferenced (condition);

      Self : Channels renames data.Self.Channels;

   begin
      Self.Stderr_Lock := @ - 1;

      Self.Process.Emit_Stderr_Available;

      if Self.Stderr_Lock = 0 then
         Self.Stderr_Event := Glib.Main.No_Source_Id;
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
      pragma Unreferenced (condition, source);

      Self : Channels renames data.Self.Channels;

   begin
      Self.Stdout_Lock := @ - 1;

      Self.Process.Emit_Stdout_Available;

      if Self.Stdout_Lock = 0 then
         Self.Stdout_Event := Glib.Main.No_Source_Id;
      end if;

      return Self.Stdout_Lock;
   end On_Stdout_Event;

   -----------------
   -- Read_Stderr --
   -----------------

   procedure Read_Stderr
     (Self : in out Channels;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Glib.Gsize;

      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Read_Chars
          (Self       => Self.Stderr_Parent,
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

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

            Start_Stderr_Watch (Self);

         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
      end case;
   end Read_Stderr;

   -----------------
   -- Read_Stdout --
   -----------------

   procedure Read_Stdout
     (Self : in out Channels;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Glib.Gsize;

      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Read_Chars
          (Self =>
             (if Self.Stdout_Parent /= null
                then Self.Stdout_Parent else Self.PTY_Channel),
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

   begin
      --  Protect against uninitialized value
      Last := Data'First - 1;

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
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
      end case;
   end Read_Stdout;

   --------------------
   -- Setup_Channels --
   --------------------

   procedure Setup_Channels
     (Self                : in out Channels;
      Standard_Input_PTY  : Boolean;
      Standard_Output_PTY : Boolean)
   is

      procedure Setup_Pipe
        (Read    : out Glib.IOChannel.Giochannel;
         Write   : out Glib.Gint;
         Success : in out Boolean);

      procedure Setup_Pipe
        (Read    : out Glib.Gint;
         Write   : out Glib.IOChannel.Giochannel;
         Success : in out Boolean);

      procedure Setup_PTY (Success : in out Boolean);

      ----------------
      -- Setup_Pipe --
      ----------------

      procedure Setup_Pipe
        (Read    : out Glib.IOChannel.Giochannel;
         Write   : out Glib.Gint;
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
            Glib.IOChannel.Unref (Read);
            Ignore := Spawn.Posix.close (Interfaces.C.int (Write));

            Read  := null;
            Write := -1;
         end Cleanup;

         Fds   : Spawn.Posix.Fd_Pair;
         Error : aliased Glib.Error.GError;

      begin
         Read  := null;
         Write := -1;

         if not Success then
            return;
         end if;

         --  Create pipe

         if Spawn.Posix.pipe2 (Fds, Posix.O_CLOEXEC) /= 0 then
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
            Success := False;

            return;
         end if;

         --  Create GIOChannel and move ownership of the file descriptor to
         --  the channel

         Read :=
           Glib.IOChannel.Giochannel_Unix_New
             (Glib.Gint (Fds (Spawn.Posix.Read_End)));
         Glib.IOChannel.Set_Close_On_Unref (Read, True);

         Write := Glib.Gint (Fds (Spawn.Posix.Write_End));

         --  Setup non-blocking mode for the channel

         if Glib.IOChannel.Set_Flags
           (Read,
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

         if Glib.IOChannel.Set_Encoding (Read, "", Error'Access)
           /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Disable buffering.

         Glib.IOChannel.Set_Buffered (Read, False);
      end Setup_Pipe;

      ----------------
      -- Setup_Pipe --
      ----------------

      procedure Setup_Pipe
        (Read    : out Glib.Gint;
         Write   : out Glib.IOChannel.Giochannel;
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
            Glib.IOChannel.Unref (Write);
            Ignore := Spawn.Posix.close (Interfaces.C.int (Read));

            Read  := -1;
            Write := null;
         end Cleanup;

         Fds   : Spawn.Posix.Fd_Pair;
         Error : aliased Glib.Error.GError;

      begin
         Read  := -1;
         Write := null;

         if not Success then
            return;
         end if;

         --  Create pipe

         if Spawn.Posix.pipe2 (Fds, Posix.O_CLOEXEC) /= 0 then
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
            Success := False;

            return;
         end if;

         --  Create GIOChannel and move ownership of the file descriptor to
         --  the channel

         Read := Glib.Gint (Fds (Spawn.Posix.Read_End));

         Write :=
           Glib.IOChannel.Giochannel_Unix_New
             (Glib.Gint (Fds (Spawn.Posix.Write_End)));
         Glib.IOChannel.Set_Close_On_Unref (Write, True);

         --  Setup non-blocking mode for the channel

         if Glib.IOChannel.Set_Flags
           (Write,
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

         if Glib.IOChannel.Set_Encoding (Write, "", Error'Access)
           /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Disable buffering.

         Glib.IOChannel.Set_Buffered (Write, False);
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

            if Self.PTY_Channel /= null then
               Glib.IOChannel.Unref (Self.PTY_Channel);
               Self.PTY_Channel := null;
            end if;
         end Cleanup;

      begin
         Self.PTY_Channel := null;
         Self.PTY_Slave   := -1;

         if not Success then
            return;
         end if;

         --  Open pseudoterminal's master descriptor

         PTY_Master :=
           Spawn.Posix.posix_openpt (Spawn.Posix.O_RDWR);

         if PTY_Master = -1 then
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));

            Success := False;

            return;
         end if;

         --  Mark file descriptor as be closed on exec

         if Spawn.Posix.fcntl
           (PTY_Master, Spawn.Posix.F_SETFD, Spawn.Posix.FD_CLOEXEC)
           = -1
         then
            Cleanup;
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
            Success := False;

            return;
         end if;

         --  Change mode and owner of the slave pseudoterminal device

         if Spawn.Posix.grantpt (PTY_Master) /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
            Success := False;

            return;
         end if;

         --  Unlock slave pseudoterminal device

         if Spawn.Posix.unlockpt (PTY_Master) /= 0 then
            Cleanup;
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
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
            Self.Process.Emit_Error_Occurred
              (Integer (System.OS_Interface.errno));
            Success := False;

            return;
         end if;

         --  Create GIOChannel and move ownership of the file descriptor to
         --  the channel

         Self.PTY_Channel :=
           Glib.IOChannel.Giochannel_Unix_New (Glib.Gint (PTY_Master));
         Glib.IOChannel.Set_Close_On_Unref (Self.PTY_Channel, True);
         PTY_Master := -1;

         --  Setup non-blocking mode for the channel

         if Glib.IOChannel.Set_Flags
           (Self.PTY_Channel,
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

         if Glib.IOChannel.Set_Encoding (Self.PTY_Channel, "", Error'Access)
           /= Glib.IOChannel.G_Io_Status_Normal
         then
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
            Cleanup;
            Success := False;

            return;
         end if;

         --  Disable buffering.

         Glib.IOChannel.Set_Buffered (Self.PTY_Channel, False);
      end Setup_PTY;

      Success : Boolean := True;

   begin
      if Standard_Input_PTY or Standard_Output_PTY then
         Setup_PTY (Success);
      end if;

      if not Standard_Input_PTY then
         Setup_Pipe
           (Self.Stdin_Child,
            Self.Stdin_Parent,
            Success);
      end if;

      if not Standard_Output_PTY then
         Setup_Pipe
           (Self.Stdout_Parent,
            Self.Stdout_Child,
            Success);
      end if;

      Setup_Pipe
        (Self.Stderr_Parent,
         Self.Stderr_Child,
         Success);
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

      --  Shutdown PTY channel

      if Self.PTY_Channel /= null then
         Glib.IOChannel.Unref (Self.PTY_Channel);
         Self.PTY_Channel := null;
      end if;
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
      Start_Watch
        (Self.Stderr_Parent,
         Self.Stderr_Event,
         Self.Stderr_Lock,
         Glib.IOChannel.G_Io_In,
         On_Stderr_Event'Access,
         Self.Process.Reference'Unchecked_Access);
   end Start_Stderr_Watch;

   -----------------------
   -- Start_Stdin_Watch --
   -----------------------

   procedure Start_Stdin_Watch (Self : in out Channels) is
   begin
      Start_Watch
        ((if Self.Stdin_Parent /= null
           then Self.Stdin_Parent else Self.PTY_Channel),
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
        ((if Self.Stdout_Parent /= null
            then Self.Stdout_Parent else Self.PTY_Channel),
         Self.Stdout_Event,
         Self.Stdout_Lock,
         Glib.IOChannel.G_Io_In,
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
     (Self : in out Channels;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize;
      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Write_Chars
          (Self            =>
             (if Self.Stdin_Parent /= null
                then Self.Stdin_Parent else Self.PTY_Channel),
           Buf           => Data,
           Bytes_Written => Count'Access,
           Error         => Error'Access);

   begin
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

      case Status is
         when Glib.IOChannel.G_Io_Status_Normal =>
            null;

         when Glib.IOChannel.G_Io_Status_Again =>
            --  There is no enough space in the buffer to write, so start
            --  watching again

            Start_Stdin_Watch (Self);

         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Process.Emit_Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));

         when others =>
            raise Program_Error;
      end case;
   end Write_Stdin;

end Spawn.Channels;
