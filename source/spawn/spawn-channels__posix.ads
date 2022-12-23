--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;
with Interfaces.C;

limited with Spawn.Internal;
with Spawn.Common;
with Spawn.Polls;

private
package Spawn.Channels is

   type Channels
     (Process : not null access Spawn.Internal.Process'Class) is private;

   type Pipe_Array is array (Spawn.Common.Pipe_Kinds) of Interfaces.C.int;
   --  File descriptors array

   procedure Setup_Channels
     (Self    : in out Channels;
      Use_PTY : Common.Pipe_Flags;
      Child   : out Pipe_Array;
      Success : out Boolean);

   procedure Close_Child_Descriptors
     (Self    : in out Channels;
      Success : out Boolean);

   procedure Close_Parent_Descriptors
     (Self    : in out Channels;
      Success : out Boolean);

   procedure Close_Parent_Descriptor
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Poll : Spawn.Polls.Poll_Access);

   procedure Start_Watch
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Poll : Spawn.Polls.Poll_Access);

   procedure Write_Stdin
     (Self : in out Channels;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Read
     (Self : in out Channels;
      Kind : Spawn.Common.Pipe_Kinds;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   function Is_Active (Self : Channels) return Boolean;

private
   Invalid : Interfaces.C.int := Interfaces.C.int'First;

   type Channels (Process : not null access Spawn.Internal.Process'Class) is
     new Spawn.Polls.Listener with
      record
         Parent     : Pipe_Array := (others => Invalid);
         Child      : Pipe_Array := (others => Invalid);
         PTY_Master : Interfaces.C.int := Invalid;
         PTY_Slave  : Interfaces.C.int := Invalid;
      end record;

   overriding procedure On_Event
     (Self   : in out Channels;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set);
   --  The handler for a new activity on the pipe descriptor.

end Spawn.Channels;
