--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Spawn several subprocess and wait all of them is finished.

with Ada.Command_Line;
with Ada.Directories;

with Spawn.String_Vectors;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;

procedure Wait_All is
   use all type Spawn.Process_Status;

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Process : Spawn.Processes.Process;
      end record;
   end Listeners;

   Command : constant String := Ada.Directories.Full_Name
     (Ada.Command_Line.Command_Name);

   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   List : array (1 .. 10) of aliased Listeners.Listener;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      --  Child process: wait next second boundary and exit
      delay 0.5;
      return;
   end if;

   Args.Append ("wait");

   for Item of List loop
      Item.Process.Set_Program (Command);
      Item.Process.Set_Arguments (Args);
      Item.Process.Set_Listener (Item'Unchecked_Access);
      Item.Process.Start;
   end loop;

   for J in 1 .. 15 loop --  J should be > 10 because we check processes
      --  stopping one by one.
      Spawn.Processes.Monitor_Loop (0.001);
      delay 0.5;

      if (for all Item of List => Item.Process.Status = Not_Running) then
         --  Success
         return;
      end if;
   end loop;

   --  Some process is till running
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Wait_All;
