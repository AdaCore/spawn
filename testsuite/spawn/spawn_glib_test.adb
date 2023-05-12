--
--  Copyright (C) 2018-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Ada.Streams;

with Spawn.Processes;
with Spawn.String_Vectors;

with Glib.Application;

procedure Spawn_Glib_Test is

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class);

   package Listeners is
      type Listener is new Spawn.Processes.Process_Listener with record
         Stopped : Boolean := False;
         App     : Glib.Application.Gapplication;
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

   P : Spawn.Processes.Process;
   L : aliased Listeners.Listener;

   package body Listeners is

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. 16);
         Last : Ada.Streams.Stream_Element_Count;
         Ok   : Boolean := True;
      begin
         Ada.Text_IO.Put_Line ("Standard_Output_Available");
         loop
            P.Read_Standard_Output (Data, Last, Ok);

            exit when Last in 0;

            for X of Data (1 .. Last) loop
               Ada.Text_IO.Put (Character'Val (X));
            end loop;

            if Last >= 2
              and then Character'Val (Data (1)) = 'O'
              and then Character'Val (Data (2)) = 'K'
            then
               P.Close_Standard_Input;
            end if;
         end loop;
      end Standard_Output_Available;

      overriding procedure Standard_Error_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Standard_Error_Available");
      end Standard_Error_Available;

      overriding procedure Standard_Input_Available
        (Self : in out Listener)
      is
         pragma Unreferenced (Self);
         Data : constant Ada.Streams.Stream_Element_Array :=
           (1 => Character'Pos ('O'),
            2 => Character'Pos ('K'),
            3 => 10);
         Last : Ada.Streams.Stream_Element_Count;
         Ok   : Boolean := True;
      begin
         Ada.Text_IO.Put_Line ("Standard_Input_Available");
         P.Write_Standard_Input (Data, Last, Ok);
      end Standard_Input_Available;

      overriding procedure Started (Self : in out Listener) is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Started");
      end Started;

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code) is
      begin
         Ada.Text_IO.Put_Line ("Finished" & (Exit_Code'Img));
         Self.Stopped := True;
         Self.App.Release;
      end Finished;

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
         pragma Unreferenced (Self);
      begin
         Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
      end Error_Occurred;

   end Listeners;

   -----------------------
   -- Activate_Callback --
   -----------------------

   procedure Activate_Callback
     (Application : access Glib.Application.Gapplication_Record'Class)
   is
      Args : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      L.App := Application.all'Access;
      Application.Hold;
      Args.Append ("/tmp/aaa.txt");
--      P.Set_Program ("/bin/echo");
      P.Set_Program ("/usr/bin/tee");
      P.Set_Arguments (Args);
      P.Set_Working_Directory ("/tmp");
      P.Set_Listener (L'Unchecked_Access);
      P.Start;
   end Activate_Callback;

   App  : constant Glib.Application.Gapplication :=
     Glib.Application.Gapplication_New
       (Flags => Glib.Application.G_Application_Flags_None);
   X : Glib.Gint;
   pragma Unreferenced (X);
begin
   App.On_Activate (Activate_Callback'Unrestricted_Access);
   X := App.Run;
end Spawn_Glib_Test;
