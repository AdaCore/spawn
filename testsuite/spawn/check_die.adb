--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Spawn self executable and check that we have all output when process die

with Ada.Command_Line;
with Ada.Directories;

with Ada.Streams;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Spawn.String_Vectors;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;

procedure Check_Die is
   use all type Spawn.Process_Status;

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Process : Spawn.Processes.Process;
         Output  : Unbounded_String;
      end record;

      overriding procedure Standard_Output_Available (Self : in out Listener);
   end Listeners;

   Command : constant String := Ada.Directories.Full_Name
     (Ada.Command_Line.Command_Name);

   Args    : Spawn.String_Vectors.UTF_8_String_Vector;

   ---------------
   -- Listeners --
   ---------------

   package body Listeners is

      -------------------------------
      -- Standard_Output_Available --
      -------------------------------

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
         Last : Ada.Streams.Stream_Element_Count;
         Ok   : Boolean := True;

      begin
         Self.Process.Read_Standard_Output (Data, Last, Ok);

         for X of Data (1 .. Last) loop
            Append (Self.Output, Character'Val (X));
         end loop;
      end Standard_Output_Available;

   end Listeners;

   Listener : aliased Listeners.Listener;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      Ada.Text_IO.Put ("help");
      return;
   end if;

   Args.Append ("second");
   Listener.Process.Set_Program (Command);
   Listener.Process.Set_Arguments (Args);
   Listener.Process.Set_Listener (Listener'Unchecked_Access);
   Listener.Process.Start;

   for J in 1 .. 6 loop
      Spawn.Processes.Monitor_Loop (0.001);
      delay 0.5;

      exit when Listener.Process.Status = Not_Running;
   end loop;

   if Listener.Output /= "help" then
      raise Program_Error;
   end if;
end Check_Die;
