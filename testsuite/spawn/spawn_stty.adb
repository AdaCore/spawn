--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Call `/bin/stty' to check if TTY support works

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.String_Vectors;

procedure Spawn_STTY is
   pragma Assertion_Policy (Check);

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Process : Spawn.Processes.Process;
         Stdin   : Ada.Strings.Unbounded.Unbounded_String;
         Stdout  : Ada.Strings.Unbounded.Unbounded_String;
         Stderr  : Ada.Strings.Unbounded.Unbounded_String;
         Started : Boolean := False;
         Stopped : Boolean := False;
         Error   : Integer := 0;
      end record;

      overriding procedure Standard_Output_Available
        (Self : in out Listener);

      overriding procedure Standard_Error_Available
        (Self : in out Listener);

      overriding procedure Started (Self : in out Listener);

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

   end Listeners;

   package body Listeners is

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         use type Ada.Streams.Stream_Element_Count;
      begin
         loop
            declare
               Data    : Ada.Streams.Stream_Element_Array (1 .. 5);
               Last    : Ada.Streams.Stream_Element_Count;
               Success : Boolean := True;

            begin
               Self.Process.Read_Standard_Output (Data, Last, Success);

               exit when Last < Data'First;

               for Char of Data (1 .. Last) loop
                  if Char not in 16#0D# | 16#0A# then
                     Ada.Strings.Unbounded.Append
                       (Self.Stdout, Character'Val (Char));
                  end if;
               end loop;
            end;
         end loop;
      end Standard_Output_Available;

      overriding procedure Standard_Error_Available
        (Self : in out Listener)
      is
         use type Ada.Streams.Stream_Element_Count;
      begin
         loop
            declare
               Data    : Ada.Streams.Stream_Element_Array (1 .. 5);
               Last    : Ada.Streams.Stream_Element_Count;
               Success : Boolean := True;

            begin
               Self.Process.Read_Standard_Error (Data, Last, Success);

               exit when Last < Data'First;

               for Char of Data (1 .. Last) loop
                  if Char not in 16#0D# | 16#0A# then
                     Ada.Strings.Unbounded.Append
                       (Self.Stderr, Character'Val (Char));
                  end if;
               end loop;
            end;
         end loop;
      end Standard_Error_Available;

      overriding procedure Started (Self : in out Listener) is
      begin
         Self.Started := True;
      end Started;

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code)
      is
         use type Spawn.Processes.Process_Exit_Code;

      begin
         if Exit_Code /= 0 then
            Ada.Text_IO.Put_Line ("Unexpected exit code" & (Exit_Code'Img));
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;

         Self.Stopped := True;
      end Finished;

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
      begin
         Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

         Self.Error := Process_Error;
         Self.Stopped := True;
      end Error_Occurred;

   end Listeners;

   Command : constant String := "/bin/stty";
   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   L       : aliased Listeners.Listener;
   P : Spawn.Processes.Process renames L.Process;
begin
   Args.Append ("-a");

   P.Set_Standard_Input_PTY;
   P.Set_Standard_Output_PTY;
   P.Set_Standard_Error_PTY;
   P.Set_Program (Command);
   P.Set_Arguments (Args);
   P.Set_Listener (L'Unchecked_Access);
   P.Start;

   while not L.Stopped loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   declare
      Stdout : constant String := Ada.Strings.Unbounded.To_String (L.Stdout);
      Stderr : constant String := Ada.Strings.Unbounded.To_String (L.Stderr);
   begin
      Ada.Text_IO.Put_Line (Stderr);
      Ada.Text_IO.Put_Line (Stdout);
      pragma Assert (Ada.Strings.Fixed.Count (Stdout, "speed") = 1);
      pragma Assert (Stderr = "");
      pragma Assert (L.Started);
      pragma Assert (L.Error = 0);
   end;
end Spawn_STTY;
