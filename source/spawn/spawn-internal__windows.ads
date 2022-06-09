--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Finalization;
with Ada.Streams;

with Spawn.Windows_API;
pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

package Spawn.Internal is
   pragma Preelaborate;

   package Environments is
      function To_Key (Text : UTF_8_String) return Wide_String;

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;
   end Environments;

   type Process is tagged;

   type Pipe_Kinds is (Stdin, Stdout, Stderr);

   Buffer_Size : constant Ada.Streams.Stream_Element_Count := 512;

   subtype Stream_Element_Buffer is
     Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);

   type Context is limited record
      lpOverlapped : Windows_API.OVERLAPPED;
      Process      : access Spawn.Internal.Process'Class;
      Kind         : Pipe_Kinds;
      Handle       : Windows_API.HANDLE := System.Win32.INVALID_HANDLE_VALUE;
      Buffer       : Stream_Element_Buffer;
      Last         : Ada.Streams.Stream_Element_Count := 0 with Atomic;
      --  If Last = 0 that means the Buffer is free and no I/O operation in
      --  progress.
      --  If Last in Buffer'Range that means a I/O operation in progress
      --  (we are writing Buffer (1 .. Last) or we have filled it during the
      --  low-level read).
      --  For Stdin, when Last > Buffer'Last that means write operation in
      --  progress (for Buffer (1 .. Last-Buffer'Length)) and we should send a
      --  notification on complete.

   end record;

   type Pipe_Array is array (Pipe_Kinds) of aliased Context;
   --  Context for each pipe kind

   type Process is
     abstract new Ada.Finalization.Limited_Controlled with record
      pid   : aliased Windows_API.PROCESS_INFORMATION;
      pipe  : Pipe_Array;
      Index : Natural := 0;
   end record;

   procedure Emit_Stdin_Available (Self : in out Process) is abstract;

   procedure Emit_Stdout_Available (Self : in out Process) is abstract;

   procedure Emit_Stderr_Available (Self : in out Process) is abstract;

   procedure Emit_Error_Occurred
     (Self          : in out Process;
      Process_Error : Integer) is abstract;

end Spawn.Internal;
