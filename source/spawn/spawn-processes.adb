--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Spawn.Processes is

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Self.Interal.Close_Standard_Error;
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Self.Interal.Close_Standard_Input;
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Self.Interal.Close_Standard_Output;
   end Close_Standard_Output;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
   begin
      Self.Interal.Kill_Process;
   end Kill_Process;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Self.Interal.Read_Standard_Error (Data, Last);
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Self.Interal.Read_Standard_Output (Data, Last);
   end Read_Standard_Output;

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Interal.Set_Arguments (Arguments);
   end Set_Arguments;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment) is
   begin
      Self.Interal.Set_Environment (Environment);
   end Set_Environment;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Spawn.Process_Listeners.Process_Listener_Access) is
   begin
      Self.Interal.Set_Listener (Listener);
   end Set_Listener;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String) is
   begin
      Self.Interal.Set_Program (Program);
   end Set_Program;

   ----------------------------
   -- Set_Standard_Error_PTY --
   ----------------------------

   procedure Set_Standard_Error_PTY (Self : in out Process'Class) is
   begin
      Self.Interal.Set_Standard_Error_PTY;
   end Set_Standard_Error_PTY;

   ----------------------------
   -- Set_Standard_Input_PTY --
   ----------------------------

   procedure Set_Standard_Input_PTY (Self : in out Process'Class) is
   begin
      Self.Interal.Set_Standard_Input_PTY;
   end Set_Standard_Input_PTY;

   -----------------------------
   -- Set_Standard_Output_PTY --
   -----------------------------

   procedure Set_Standard_Output_PTY (Self : in out Process'Class) is
   begin
      Self.Interal.Set_Standard_Output_PTY;
   end Set_Standard_Output_PTY;

   ---------------------------
   -- Set_Working_Directory --
   ---------------------------

   procedure Set_Working_Directory
     (Self : in out Process'Class; Directory : UTF_8_String) is
   begin
      Self.Interal.Set_Working_Directory (Directory);
   end Set_Working_Directory;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Interal.Start;
   end Start;

   -----------------------
   -- Terminate_Process --
   -----------------------

   procedure Terminate_Process (Self : in out Process'Class) is
   begin
      Self.Interal.Terminate_Process;
   end Terminate_Process;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Self.Interal.Write_Standard_Input (Data, Last);
   end Write_Standard_Input;

end Spawn.Processes;
