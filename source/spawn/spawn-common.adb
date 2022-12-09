--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Spawn.Common is

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
      Listener : Spawn.Process_Listeners.Process_Listener_Access) is
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
      Self.Command := Ada.Strings.Unbounded.To_Unbounded_String (Program);
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
     (Self : in out Process'Class; Directory : UTF_8_String) is
   begin
      Self.Directory := Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Working_Directory;

end Spawn.Common;
