--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Process implementation for POSIX without Glib integration.

with Ada.Streams;
with Interfaces.C;

with Spawn.Common;
with Spawn.Channels;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   procedure Loop_Cycle (Timeout : Duration);
   --  See Spawn.Internal.Monitor

   subtype Pipe_Kinds is Spawn.Common.Pipe_Kinds;

   type Pipe_Array is array (Pipe_Kinds) of Interfaces.C.int;
   --  File descriptors array

   type Process is new Spawn.Common.Process with private;
   --  Process implementation type provides the same interface as
   --  Spawn.Processes.Process type.

   procedure Start (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Terminate_Process (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   procedure Kill_Process (Self : in out Process'Class);
   --  See documentation in Spawn.Processes.

   function Identifier (Self : Process'Class) return String;
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

private

   type Process is new Spawn.Common.Process with record
      pid      : Interfaces.C.int := 0;
      Channels : aliased Spawn.Channels.Channels (Process'Unchecked_Access);
   end record;

   overriding procedure Finalize (Self : in out Process);

end Spawn.Internal;
