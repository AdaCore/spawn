--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Base type for process implementation on all platforms.

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with Spawn.Environments;
with Spawn.Process_Listeners;
with Spawn.String_Vectors;

private
package Spawn.Common is

   type Pipe_Kinds is (Launch, Stdout, Stderr, Stdin);

   subtype Standard_Pipe is Pipe_Kinds range Stdout .. Stdin;

   type Pipe_Flags is array (Standard_Pipe) of Boolean;

   type Process is new Ada.Finalization.Limited_Controlled with record
      Arguments   : Spawn.String_Vectors.UTF_8_String_Vector;
      Environment : Spawn.Environments.Process_Environment :=
        Spawn.Environments.System_Environment;
      Exit_Status : Process_Exit_Status := Normal;
      Exit_Code   : Process_Exit_Code := Process_Exit_Code'Last;
      Status      : Process_Status := Not_Running;

      Listener    : Spawn.Process_Listeners.Process_Listener_Access;
      --  The associated listener. Note: this may be null.

      Command     : Ada.Strings.Unbounded.Unbounded_String;
      Directory   : Ada.Strings.Unbounded.Unbounded_String;
      Use_PTY     : Pipe_Flags := (others => False);
   end record;

   function Arguments (Self : Process'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector is
       (Self.Arguments);
   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector);

   function Environment (Self : Process'Class)
     return Spawn.Environments.Process_Environment is
       (Self.Environment);
   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment);

   function Working_Directory (Self : Process'Class) return UTF_8_String is
     (Ada.Strings.Unbounded.To_String (Self.Directory));
   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String);

   function Program (Self : Process'Class) return UTF_8_String is
     (Ada.Strings.Unbounded.To_String (Self.Command));
   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String);

   procedure Set_Standard_Input_PTY (Self : in out Process'Class);

   procedure Set_Standard_Output_PTY (Self : in out Process'Class);

   procedure Set_Standard_Error_PTY (Self : in out Process'Class);

   procedure Emit_Started (Self : Process'Class);
   procedure Emit_Stdin_Available (Self : Process'Class);
   procedure Emit_Stderr_Available (Self : Process'Class);
   procedure Emit_Stdout_Available (Self : Process'Class);

   procedure Emit_Error_Occurred
     (Self          : Process'Class;
      Process_Error : Integer);

   procedure Emit_Exception_Occurred
     (Self       : Process'Class;
      Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Emit_Finished
     (Self        : Process'Class;
      Exit_Status : Process_Exit_Status;
      Exit_Code   : Process_Exit_Code);

   function Status (Self : Process'Class) return Process_Status is
     (Self.Status);

   function Exit_Status (Self : Process'Class) return Process_Exit_Status is
     (Self.Exit_Status);
   --  Return the exit status of last process that finishes.

   function Exit_Code (Self : Process'Class) return Process_Exit_Code is
     (Self.Exit_Code);

   function Listener (Self : Process'Class)
     return Spawn.Process_Listeners.Process_Listener_Access is
       (Self.Listener);

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Spawn.Process_Listeners.Process_Listener_Access);

end Spawn.Common;
