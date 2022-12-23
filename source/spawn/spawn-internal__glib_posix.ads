--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;
with Interfaces.C;

with Glib.Main;

with Spawn.Channels;
with Spawn.Common;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Process is tagged;

   type Process_Reference is record
      Self : access Process'Class;
   end record;
   --  A wrapper to pass process pointer to C binding functions

   type Process is new Spawn.Common.Process with record
      Reference : aliased Process_Reference;
      Channels  : Spawn.Channels.Channels (Process'Unchecked_Access);
      Event     : Glib.Main.G_Source_Id := 0;
      pid       : Interfaces.C.int := 0;
      Dummy     : Interfaces.C.int := 0;
      --  GtkAda declares (incorrectly?) GPid type as 64-bit integer
   end record;

   overriding procedure Finalize (Self : in out Process);

   procedure Start (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Terminate_Process (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Kill_Process (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Close_Standard_Input (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  See documentation in Spawn.Processes.

   procedure Close_Standard_Output (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  See documentation in Spawn.Processes.

   procedure Close_Standard_Error (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  See documentation in Spawn.Processes.

end Spawn.Internal;
