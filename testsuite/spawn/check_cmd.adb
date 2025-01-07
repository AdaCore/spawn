--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Spawn self executable and check that command line arguments are valid

with Ada.Command_Line;
with Ada.Directories;

with Spawn.String_Vectors;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;

procedure Check_Cmd is
   use all type Spawn.Process_Status;

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Process : Spawn.Processes.Process;
      end record;
   end Listeners;

   Command : constant String := Ada.Directories.Full_Name
     (Ada.Command_Line.Command_Name);

   Args     : Spawn.String_Vectors.UTF_8_String_Vector;
   Listener : aliased Listeners.Listener;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      --  Child process: wait next second boundary and exit
      delay 0.5;

      if Ada.Command_Line.Argument (2) = "/c"
        and then Ada.Command_Line.Argument (3) = "start"
        and then Ada.Command_Line.Argument (4) = """exe name"""
      then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      return;
   end if;

   Args.Append ("/c");
   Args.Append ("start");
   Args.Append ("exe name");

   Listener.Process.Set_Program (Command);
   Listener.Process.Set_Arguments (Args);
   Listener.Process.Set_Listener (Listener'Unchecked_Access);
   Listener.Process.Start;

   for J in 1 .. 6 loop
      Spawn.Processes.Monitor_Loop (0.001);
      delay 0.5;

      if Listener.Process.Status = Not_Running then
         --  Success
         return;
      end if;
   end loop;

   --  Some process is till running
   Ada.Command_Line.Set_Exit_Status
     (Ada.Command_Line.Exit_Status (Listener.Process.Exit_Code));
end Check_Cmd;
