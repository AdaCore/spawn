--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Hashed_Maps;

package Spawn.Polls.POSIX_Polls is

   type POSIX_Poll is new Poll with private;

private

   type Info is record
      Events   : Watch_Event_Set;
      Listener : Listener_Access;
   end record;

   function Hash (V : Descriptor) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (V));

   package Info_Maps is new Ada.Containers.Hashed_Maps
     (Descriptor, Info, Hash, Interfaces.C."=");

   type POSIX_Poll is new Poll with record
      Initialized : Boolean := False;
      Map         : Info_Maps.Map;
   end record;

   overriding function Is_Initialized (Self : POSIX_Poll) return Boolean;

   overriding procedure Initialize (Self : out POSIX_Poll);

   overriding procedure Watch
     (Self     : in out POSIX_Poll;
      Value    : Descriptor;
      Events   : Watch_Event_Set;
      Listener : Listener_Access := null);

   overriding procedure Wait
     (Self    : in out POSIX_Poll;
      Timeout : Duration);

end Spawn.Polls.POSIX_Polls;
