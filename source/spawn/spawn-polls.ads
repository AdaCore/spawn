--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C;

package Spawn.Polls is
   pragma Preelaborate;

   type Poll is limited interface;
   --  The poll waits for one of a set of file descriptors to become ready to
   --  perform I/O.

   type Poll_Access is access all Poll'Class with Storage_Size => 0;

   type Listener is limited interface;
   type Listener_Access is access all Listener'Class with Storage_Size => 0;

   type Event is (Input, Output, Error, Close);
   subtype Watch_Event is Event range Input .. Output;
   --  Configurable event to watch
   type Event_Set is array (Event range <>) of Boolean with Pack;
   subtype Watch_Event_Set is Event_Set (Watch_Event);

   function Empty_Set return Watch_Event_Set is (False, False);
   function Input     return Watch_Event_Set is (True, False);
   function Output    return Watch_Event_Set is (False, True);

   subtype Descriptor is Interfaces.C.int;  --  File, Pipe or Socket descriptor

   not overriding procedure On_Event
     (Self   : in out Listener;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set) is null;
   --  Report an event during Wait call. The poll wan't watch events on the
   --  descriptor until next Watch call.

   function Is_Initialized (Self : Poll) return Boolean is abstract;

   procedure Initialize (Self : out Poll) is abstract
     with Pre'Class => not Self.Is_Initialized,
          Post'Class => Self.Is_Initialized;

   procedure Watch
     (Self     : in out Poll;
      Value    : Descriptor;
      Events   : Watch_Event_Set;
      Listener : Listener_Access := null) is abstract
     with Pre'Class => Self.Is_Initialized and then
       not (Listener = null xor Events = Empty_Set);
   --  If Events isn't an empty set, then start watching events on the
   --  descriptor or change event set/listener if the descriptor is already
   --  watched by the poll. If Events is empty, then stop watch.

   procedure Wait
     (Self    : in out Poll;
      Timeout : Duration) is abstract
     with Pre'Class => Self.Is_Initialized;

end Spawn.Polls;
