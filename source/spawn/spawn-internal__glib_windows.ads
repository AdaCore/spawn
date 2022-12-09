--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Finalization;
with Ada.Streams;

with Glib.Main;
with Glib.Spawn;

with Spawn.Windows_API;
pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Common;

private package Spawn.Internal is

   package Environments is
      function To_Key (Text : UTF_8_String) return Wide_String;

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Process is tagged;

   Buffer_Size : constant Ada.Streams.Stream_Element_Count := 512;

   subtype Stream_Element_Buffer is
     Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);

   subtype Pipe_Kinds is Spawn.Common.Pipe_Kinds;

   type Context is record
      lpOverlapped : Windows_API.OVERLAPPED;
      Process      : access Spawn.Internal.Process'Class;
      Kind         : Pipe_Kinds;
      Handle       : Windows_API.HANDLE := System.Win32.INVALID_HANDLE_VALUE;
      Buffer       : Stream_Element_Buffer;
      Last         : Ada.Streams.Stream_Element_Count := 0 with Atomic;
      --  Last could be > Buffer'Last for Stdin that means 'send notification'
   end record;

   type Pipe_Array is array (Pipe_Kinds) of aliased Context;
   --  Context for each pipe kind

   type Process_Reference is record
      Self : access Process'Class;
   end record;
   --  A wrapper to pass process pointer to C binding functions

   type Process is new Spawn.Common.Process with record
      Reference : aliased Process_Reference;
      Event     : Glib.Main.G_Source_Id := 0;
      pid       : aliased Windows_API.PROCESS_INFORMATION;
      pipe      : Pipe_Array;
   end record;
   --  Process implementation type provides the same interface as
   --  Spawn.Processes.Process type.

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
