--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Spawn.String_Vectors;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;

with Signals;
pragma Unreferenced (Signals);

procedure Spawn_Kill is
   pragma Assertion_Policy (Check);

   package Listeners is
      type Listener is limited new Spawn.Processes.Process_Listener with record
         Proc    : Spawn.Processes.Process;
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
               Self.Proc.Read_Standard_Output (Data, Last, Success);
               pragma Assert (Success);

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
               Self.Proc.Read_Standard_Error (Data, Last, Success);

               exit when Last < Data'First;

               for Char of Data (1 .. Last) loop
                  if Char not in 16#0D# | 16#0A# then
                     Ada.Strings.Unbounded.Append
                       (Self.Stderr, Character'Val (Char));
                  end if;
               end loop;
            end;
         end loop;

         Self.Proc.Close_Standard_Input;
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
         if Exit_Code /= 9 then
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

         Self.Stopped := True;
         Self.Error := Process_Error;
      end Error_Occurred;

   end Listeners;

   Command : constant String := Ada.Directories.Full_Name
     (Ada.Command_Line.Command_Name);
   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   L    : aliased Listeners.Listener;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line);

      return;
   end if;

   --  Otherwise launch a driven process.
   Args.Append ("Wait for signal");

   L.Proc.Set_Program (Command);
   L.Proc.Set_Arguments (Args);
   L.Proc.Set_Working_Directory (Ada.Directories.Current_Directory);
   L.Proc.Set_Listener (L'Unchecked_Access);
   L.Proc.Start;

   while not L.Started loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   L.Proc.Terminate_Process;

   while Ada.Strings.Unbounded.Length (L.Stdout) = 0 loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   L.Proc.Kill_Process;

   while not L.Stopped loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   declare
      Stdout : constant String := Ada.Strings.Unbounded.To_String (L.Stdout);
      Stderr : constant String := Ada.Strings.Unbounded.To_String (L.Stderr);
   begin
      pragma Assert (Stdout = "Got TERM");
      pragma Assert (Stderr = "");
      pragma Assert (L.Started);
      pragma Assert (L.Error = 0);
   end;
end Spawn_Kill;
