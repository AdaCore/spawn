--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Try to spawn a not executable file

with Ada.Command_Line;
with Ada.Text_IO;

with Spawn.Processes.Monitor_Loop;

procedure Spawn_Bad_Exe is

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Proc    : Spawn.Processes.Process;
         Error   : Integer := 0;
         Wrong   : Boolean := False;  --  Set in Started/Finished
      end record;

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

      overriding procedure Started (Self : in out Listener);

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Process_Exit_Status;
         Exit_Code   : Spawn.Process_Exit_Code);

   end Listeners;

   package body Listeners is

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer) is
      begin
         Self.Error := Process_Error;
      end Error_Occurred;

      overriding procedure Started (Self : in out Listener) is
      begin
         Ada.Text_IO.Put_Line ("Unexpected Started");
         Self.Wrong := True;
      end Started;

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Process_Exit_Status;
         Exit_Code   : Spawn.Process_Exit_Code) is
      begin
         Ada.Text_IO.Put_Line ("Unexpected Finished");
         Self.Wrong := True;
      end Finished;

   end Listeners;

   use all type Spawn.Process_Status;

   Command : constant String :=
     (if Ada.Command_Line.Argument_Count > 0
      then Ada.Command_Line.Argument (1)
      else ".");

   List    : array (1 .. 5) of aliased Listeners.Listener;
begin
   for X of List loop
      X.Proc.Set_Program (Command);
      X.Proc.Set_Listener (X'Unchecked_Access);
      X.Proc.Start;
   end loop;

   while (for some X of List => X.Proc.Status in Starting | Running) loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   if (for some X of List => X.Wrong or X.Error /= List (1).Error) then
      raise Program_Error;
   end if;
end Spawn_Bad_Exe;
