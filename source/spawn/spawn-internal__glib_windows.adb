--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Wide_Characters.Unicode;

with Glib.Spawn;

with Spawn.Internal.Windows;

package body Spawn.Internal is
   use type Ada.Streams.Stream_Element_Offset;
   use all type Spawn.Common.Pipe_Kinds;

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) = To_Key (Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) < To_Key (Right);
      end "<";

      ------------
      -- To_Key --
      ------------

      function To_Key (Text : UTF_8_String) return Wide_String is
         Value : Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Text);
      begin
         for Char of Value loop
            Char := Ada.Wide_Characters.Unicode.To_Upper_Case (Char);
         end loop;

         return Value;
      end To_Key;

   end Environments;

   type Process_Access is access all Process'Class;

   procedure Do_Start_Process (Self : aliased in out Process'Class);

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Spawn.Common.Standard_Pipe);

   function Child_Watch is new Glib.Main.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Collback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   package Read_Write_Ex is
     new Windows_API.Generic_Read_Write_Ex (Context);

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Windows.Do_Close_Pipe (Self, Stderr);
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Windows.Do_Close_Pipe (Self, Stdin);
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Windows.Do_Close_Pipe (Self, Stdout);
   end Close_Standard_Output;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Spawn.Common.Standard_Pipe)
   is
      procedure On_No_Data;

      ----------------
      -- On_No_Data --
      ----------------

      procedure On_No_Data is
         use type Windows_API.BOOL;

         Ok    : Windows_API.BOOL;
         Pipe  : Context renames Self.pipe (Kind);
      begin
         Ok := Read_Write_Ex.ReadFileEx
           (hFile                => Pipe.Handle,
            lpBuffer             => Pipe.Buffer,
            nNumberOfBytesToRead => Pipe.Buffer'Length,
            lpOverlapped         => Pipe'Access,
            lpCompletionRoutine  => Callback (Kind));

         if Ok = System.Win32.FALSE then
            Self.Emit_Error_Occurred (Integer (System.Win32.GetLastError));
         end if;
      end On_No_Data;

   begin
      Windows.Do_Read (Self, Data, Last, Kind, On_No_Data'Access);
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      procedure On_Start;

      procedure On_Start is
      begin
         Self.Event := Child_Watch
           (Glib.Spawn.GPid (Self.pid.hProcess),
            My_Death_Collback'Access,
            Self.Reference'Access);
      end On_Start;

   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Windows.Do_Start_Process (Self, On_Start'Access);
   end Do_Start_Process;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
      use type Glib.Main.G_Source_Id;

   begin
      --  Close stdio pipes
      for J in Self.pipe'Range loop
         Windows.Do_Close_Pipe (Self, J);
      end loop;

      if Self.Event /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Self.Event);
         Self.Event := Glib.Main.No_Source_Id;
      end if;
   end Finalize;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Process'Class) return String is
      use type Spawn.Windows_API.DWORD;

      Image : constant String := Self.pid.dwProcessId'Image;
   begin
      return (if Self.pid.dwProcessId = 0 then ""
              else Image (2 .. Image'Last));
   end Identifier;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
   begin
      Windows.Do_Kill_Process (Self);
   end Kill_Process;

   -----------------------
   -- My_Death_Collback --
   -----------------------

   procedure My_Death_Collback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
   is
      pragma Unreferenced (pid, status);

      Process : constant Process_Access := Process_Access (data.Self);
   begin
      Process.Event := Glib.Main.No_Source_Id;
      Windows.On_Process_Died (Process.all);
   end My_Death_Collback;

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

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stderr);
   end Standard_Error_Callback;

   ------------------------------
   -- Standard_Output_Callback --
   ------------------------------

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdout);
   end Standard_Output_Callback;

   -----------------------------
   -- Standard_Input_Callback --
   -----------------------------

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdin);
   end Standard_Input_Callback;

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
   begin
      Windows.Do_Terminate_Process (Self);
   end Terminate_Process;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure On_Has_Data;

      -----------------
      -- On_Has_Data --
      -----------------

      procedure On_Has_Data is
         use type Windows_API.BOOL;

         Ok   : Windows_API.BOOL;
         Pipe : Context renames Self.pipe (Stdin);

      begin
         pragma Assert (Pipe.Last /= 0);

         Ok := Read_Write_Ex.WriteFileEx
           (hFile                 => Pipe.Handle,
            lpBuffer              => Pipe.Buffer,
            nNumberOfBytesToWrite =>
              Windows_API.DWORD
                (Pipe.Last
                 - (if Pipe.Last in Pipe.Buffer'Range
                    then 0
                    else Spawn.Internal.Buffer_Size)),
            lpOverlapped          => Pipe'Access,
            lpCompletionRoutine   => Standard_Input_Callback'Access);

         if Ok = System.Win32.FALSE then
            Self.Emit_Error_Occurred (Integer (System.Win32.GetLastError));
         end if;
      end On_Has_Data;

   begin
      Windows.Do_Write (Self, Data, Last, On_Has_Data'Access);
   end Write_Standard_Input;

end Spawn.Internal;
