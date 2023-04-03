--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This is a test for Process API.
--
--  When it's called with no arguments, then it starts the same executable
--  with some arguments and pass some data to stdin. Then it waits for
--  the termination and checks stdout/stderr streams and exit status.
--
--  If it's launched with some arguments, then it enumerates arguments to the
--  stdout stream, echos one string from stdin to stderr and exits.

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Strings.Unbounded;

with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.String_Vectors;

procedure Spawn_Test is
   pragma Assertion_Policy (Check);

   P : Spawn.Processes.Process;

   package Listeners is
      type Listener is new Spawn.Processes.Process_Listener with record
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

      overriding procedure Standard_Input_Available
        (Self : in out Listener);
      --  Called once when it's possible to write data again.

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
               P.Read_Standard_Output (Data, Last, Success);

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
               P.Read_Standard_Error (Data, Last, Success);

               exit when Last < Data'First;

               for Char of Data (1 .. Last) loop
                  if Char not in 16#0D# | 16#0A# then
                     Ada.Strings.Unbounded.Append
                       (Self.Stderr, Character'Val (Char));
                  end if;
               end loop;
            end;
         end loop;

         P.Close_Standard_Input;
      end Standard_Error_Available;

      overriding procedure Standard_Input_Available
        (Self : in out Listener)
      is
         use type Ada.Streams.Stream_Element_Count;

         Text    : constant String :=
           Ada.Strings.Unbounded.To_String (Self.Stdin);
         Last    : Ada.Streams.Stream_Element_Count := Text'Length;
         Data    : Ada.Streams.Stream_Element_Array (1 .. Last);
         Success : Boolean := True;

      begin
         for J in Data'Range loop
            Data (J) := Character'Pos (Text (Positive (J)));
         end loop;

         P.Write_Standard_Input (Data, Last, Success);

         pragma Assert (Last = Data'Last);
         Self.Stdin := Ada.Strings.Unbounded.Null_Unbounded_String;
      end Standard_Input_Available;

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

   Command : constant String := Ada.Directories.Full_Name
     (Ada.Command_Line.Command_Name);
   Args : Spawn.String_Vectors.UTF_8_String_Vector;
   L    : aliased Listeners.Listener;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      --  Work as a driven process.
      for J in 1 .. Ada.Command_Line.Argument_Count loop
         Ada.Text_IO.Put (Positive'Image (J));
         Ada.Text_IO.Put (" => ");
         Ada.Text_IO.Put_Line (Ada.Command_Line.Argument (J));
      end loop;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Text_IO.Get_Line);

      return;
   end if;

   --  Otherwise launch a driven process.
   L.Stdin := Ada.Strings.Unbounded.To_Unbounded_String
     ("Stdin sample" & Ada.Characters.Latin_1.LF);
   Args.Append ("Hello_World");
   Args.Append ("space> <space");

   P.Set_Program (Command);
   P.Set_Arguments (Args);
   P.Set_Working_Directory (Ada.Directories.Current_Directory);
   P.Set_Listener (L'Unchecked_Access);
   P.Start;

   while not L.Stopped loop
      Spawn.Processes.Monitor_Loop (0.001);
   end loop;

   declare
      Stdin : constant String := Ada.Strings.Unbounded.To_String (L.Stdin);
      Stdout : constant String := Ada.Strings.Unbounded.To_String (L.Stdout);
      Stderr : constant String := Ada.Strings.Unbounded.To_String (L.Stderr);
   begin
      pragma Assert (Stdin = "");
      pragma Assert (Stderr = "Stdin sample");
      pragma Assert (Stdout = " 1 => Hello_World 2 => space> <space");
      pragma Assert (L.Started);
      pragma Assert (L.Error = 0);
   end;
end Spawn_Test;
