--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Spawn.Common is
   use type Spawn.Process_Listeners.Process_Listener_Access;

   -------------------------
   -- Emit_Error_Occurred --
   -------------------------

   procedure Emit_Error_Occurred
     (Self          : Process'Class;
      Process_Error : Integer) is
   begin
      if Self.Listener /= null then
         Self.Listener.Error_Occurred (Process_Error);
      end if;
   exception
      when others =>
         null;
   end Emit_Error_Occurred;

   -----------------------------
   -- Emit_Exception_Occurred --
   -----------------------------

   procedure Emit_Exception_Occurred
     (Self       : Process'Class;
      Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      if Self.Listener /= null then
         Self.Listener.Exception_Occurred (Occurrence);
      end if;
   exception
      when others =>
         null;
   end Emit_Exception_Occurred;

   -------------------
   -- Emit_Finished --
   -------------------

   procedure Emit_Finished
     (Self        : Process'Class;
      Exit_Status : Process_Exit_Status;
      Exit_Code   : Process_Exit_Code) is
   begin
      if Self.Listener /= null then
         Self.Listener.Finished (Exit_Status, Exit_Code);
      end if;
   exception
      when others =>
         null;
   end Emit_Finished;

   ------------------
   -- Emit_Started --
   ------------------

   procedure Emit_Started (Self : Process'Class) is
   begin
      if Self.Listener /= null then
         Self.Listener.Started;
      end if;
   exception
      when others =>
         null;
   end Emit_Started;

   ---------------------------
   -- Emit_Stderr_Available --
   ---------------------------

   procedure Emit_Stderr_Available (Self : Process'Class) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Error_Available;
      end if;
   exception
      when others =>
         null;
   end Emit_Stderr_Available;

   --------------------------------------
   -- Emit_Standard_Error_Stream_Error --
   --------------------------------------

   procedure Emit_Standard_Error_Stream_Error
     (Self    : Process'Class;
      Message : String) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Output_Stream_Error (Message);
      end if;

   exception
      when others =>
         null;
   end Emit_Standard_Error_Stream_Error;

   --------------------------------------
   -- Emit_Standard_Input_Stream_Error --
   --------------------------------------

   procedure Emit_Standard_Input_Stream_Error
     (Self    : Process'Class;
      Message : String) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Input_Stream_Error (Message);
      end if;

   exception
      when others =>
         null;
   end Emit_Standard_Input_Stream_Error;

   ---------------------------------------
   -- Emit_Standard_Output_Stream_Error --
   ---------------------------------------

   procedure Emit_Standard_Output_Stream_Error
     (Self    : Process'Class;
      Message : String) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Output_Stream_Error (Message);
      end if;

   exception
      when others =>
         null;
   end Emit_Standard_Output_Stream_Error;

   --------------------------
   -- Emit_Stdin_Available --
   --------------------------

   procedure Emit_Stdin_Available (Self : Process'Class) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Input_Available;
      end if;
   exception
      when others =>
         null;
   end Emit_Stdin_Available;

   ---------------------------
   -- Emit_Stdout_Available --
   ---------------------------

   procedure Emit_Stdout_Available (Self : Process'Class) is
   begin
      if Self.Listener /= null then
         Self.Listener.Standard_Output_Available;
      end if;
   exception
      when others =>
         null;
   end Emit_Stdout_Available;

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
