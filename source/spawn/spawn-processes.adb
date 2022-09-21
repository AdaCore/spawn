--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Spawn.Processes is

   package Platform is

      procedure Close_Standard_Error (Self : in out Process'Class);

      procedure Close_Standard_Input (Self : in out Process'Class);

      procedure Close_Standard_Output (Self : in out Process'Class);

      procedure Finalize (Self : in out Process'Class);

      procedure Kill_Process (Self : in out Process'Class);

      procedure Read_Standard_Error
        (Self : in out Process'Class;
         Data : out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Offset);

      procedure Read_Standard_Output
        (Self : in out Process'Class;
         Data : out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Offset);

      procedure Start (Self : in out Process'Class);

      procedure Terminate_Process (Self : in out Process'Class);

      procedure Write_Standard_Input
        (Self : in out Process'Class;
         Data : Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Offset);

   end Platform;

   ---------------
   -- Arguments --
   ---------------

   function Arguments
     (Self : Process'Class)
      return Spawn.String_Vectors.UTF_8_String_Vector is
   begin
      return Self.Arguments;
   end Arguments;

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class)
     renames Platform.Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class)
     renames Platform.Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class)
     renames Platform.Close_Standard_Output;

   -------------------------
   -- Emit_Error_Occurred --
   -------------------------

   overriding procedure Emit_Error_Occurred
     (Self  : in out Process;
      Error : Integer) is
   begin
      Self.Listener.Error_Occurred (Error);

   exception
      when others =>
         null;
   end Emit_Error_Occurred;

   ---------------------------
   -- Emit_Stderr_Available --
   ---------------------------

   overriding procedure Emit_Stderr_Available (Self : in out Process) is
   begin
      Self.Listener.Standard_Error_Available;

   exception
      when others =>
         null;
   end Emit_Stderr_Available;

   --------------------------
   -- Emit_Stdin_Available --
   --------------------------

   overriding procedure Emit_Stdin_Available (Self : in out Process) is
   begin
      Self.Listener.Standard_Input_Available;

   exception
      when others =>
         null;
   end Emit_Stdin_Available;

   ---------------------------
   -- Emit_Stdout_Available --
   ---------------------------

   overriding procedure Emit_Stdout_Available (Self : in out Process) is
   begin
      Self.Listener.Standard_Output_Available;

   exception
      when others =>
         null;
   end Emit_Stdout_Available;

   -----------------
   -- Environment --
   -----------------

   function Environment
     (Self : Process'Class)
      return Spawn.Environments.Process_Environment is
   begin
      return Self.Environment;
   end Environment;

   ---------------
   -- Exit_Code --
   ---------------

   function Exit_Code (Self : Process'Class) return Process_Exit_Code is
   begin
      return Self.Exit_Code;
   end Exit_Code;

   -----------------
   -- Exit_Status --
   -----------------

   function Exit_Status (Self : Process'Class) return Process_Exit_Status is
   begin
      return Self.Exit_Status;
   end Exit_Status;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
   begin
      Platform.Finalize (Self);
   end Finalize;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
   begin
      if Self.Status = Running then
         Platform.Kill_Process (Self);
      end if;
   end Kill_Process;

   --------------
   -- Listener --
   --------------

   function Listener (Self : Process'Class) return Process_Listener_Access is
   begin
      return Self.Listener;
   end Listener;

   --------------
   -- Platform --
   --------------

   package body Platform is separate;

   -----------------------
   -- On_Close_Channels --
   -----------------------

   overriding procedure On_Close_Channels (Self : in out Process) is
   begin
      if Self.Pending_Finish then
         Self.Pending_Finish := False;

         begin
            Self.Listener.Finished (Self.Exit_Status, Self.Exit_Code);

         exception
            when others =>
               null;
         end;
      end if;
   end On_Close_Channels;

   -------------
   -- Program --
   -------------

   function Program (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Program);
   end Program;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
     renames Platform.Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
     renames Platform.Read_Standard_Output;

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Arguments := Arguments;
   end Set_Arguments;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment) is
   begin
      Self.Environment := Environment;
   end Set_Environment;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Process_Listener_Access)
   is
   begin
      Self.Listener := Listener;
   end Set_Listener;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String) is
   begin
      Self.Program := Ada.Strings.Unbounded.To_Unbounded_String (Program);
   end Set_Program;

   ----------------------------
   -- Set_Standard_Error_PTY --
   ----------------------------

   procedure Set_Standard_Error_PTY (Self : in out Process'Class) is
   begin
      Self.Use_PTY (Stderr) := True;
   end Set_Standard_Error_PTY;

   ----------------------------
   -- Set_Standard_Input_PTY --
   ----------------------------

   procedure Set_Standard_Input_PTY (Self : in out Process'Class) is
   begin
      Self.Use_PTY (Stdin) := True;
   end Set_Standard_Input_PTY;

   -----------------------------
   -- Set_Standard_Output_PTY --
   -----------------------------

   procedure Set_Standard_Output_PTY (Self : in out Process'Class) is
   begin
      Self.Use_PTY (Stdout) := True;
   end Set_Standard_Output_PTY;

   ---------------------------
   -- Set_Working_Directory --
   ---------------------------

   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String) is
   begin
      Self.Directory := Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Working_Directory;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class)
     renames Platform.Start;

   ------------
   -- Status --
   ------------

   function Status (Self : Process'Class) return Process_Status is
   begin
      return Self.Status;
   end Status;

   -----------------------
   -- Terminate_Process --
   -----------------------

   procedure Terminate_Process (Self : in out Process'Class) is
   begin
      if Self.Status = Running then
         Platform.Terminate_Process (Self);
      end if;
   end Terminate_Process;

   -----------------------
   -- Working_Directory --
   -----------------------

   function Working_Directory (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Directory);
   end Working_Directory;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
     renames Platform.Write_Standard_Input;

end Spawn.Processes;
