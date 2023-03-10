--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNAT.OS_Lib;

with Spawn.Posix;

package body Spawn.Polls.POSIX_Polls is

   function To_Event_Set
     (Set : Watch_Event_Set) return Interfaces.C.unsigned_short;

   function To_Event_Set (Set : Interfaces.C.unsigned_short) return Event_Set;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : out POSIX_Poll) is
   begin
      Self.Initialized := True;
   end Initialize;

   --------------------
   -- Is_Initialized --
   --------------------

   overriding function Is_Initialized (Self : POSIX_Poll) return Boolean is
   begin
      return Self.Initialized;
   end Is_Initialized;

   ------------------
   -- To_Event_Set --
   ------------------

   function To_Event_Set
     (Set : Watch_Event_Set) return Interfaces.C.unsigned_short
   is
      use type Interfaces.C.unsigned_short;

      Map : constant array (Watch_Event) of Interfaces.C.unsigned_short :=
        (Input  => Posix.POLLIN,
         Output => Posix.POLLOUT);

      Result : Interfaces.C.unsigned_short := 0;
   begin
      for J in Set'Range loop
         if Set (J) then
            Result := Result + Map (J);
         end if;
      end loop;

      return Result;
   end To_Event_Set;

   ------------------
   -- To_Event_Set --
   ------------------

   function To_Event_Set
     (Set : Interfaces.C.unsigned_short) return Event_Set
   is
      use type Interfaces.C.unsigned_short;

      Map : constant array (Event) of Interfaces.C.unsigned_short :=
        (Close  => Posix.POLLHUP,
         Input  => Posix.POLLIN,
         Output => Posix.POLLOUT,
         others => 0);

      Value  : Interfaces.C.unsigned_short := Set;
      Result : Event_Set := (Event => False);
   begin
      for J in Map'Range loop
         if (Value and Map (J)) /= 0 then
            Value := Value - Map (J);
            Result (J) := True;
         end if;
      end loop;

      Result (Error) := Value /= 0;

      return Result;
   end To_Event_Set;

   -----------
   -- Watch --
   -----------

   overriding procedure Watch
     (Self     : in out POSIX_Poll;
      Value    : Descriptor;
      Events   : Watch_Event_Set;
      Listener : Listener_Access := null)
   is
      Cursor : constant Info_Maps.Cursor := Self.Map.Find (Value);
   begin
      if Events = Empty_Set then
         Self.Map.Exclude (Value);
      elsif Info_Maps.Has_Element (Cursor) then
         Self.Map (Cursor) := (Events, Listener);
      else
         Self.Map.Insert (Value, (Events, Listener));
      end if;
   end Watch;

   ----------
   -- Wait --
   ----------

   overriding procedure Wait
     (Self    : in out POSIX_Poll;
      Timeout : Duration)
   is
      use type Interfaces.C.int;
      use type Interfaces.C.unsigned_short;

      Length   : constant Natural := Natural (Self.Map.Length);
      Index    : Positive := 1;
      fds      : Posix.pollfd_array (1 .. Length);
      Listener : Listener_Access;
      m_sec : constant Interfaces.C.int := Interfaces.C.int (Timeout * 1000.0);
      --  Wait for an event in the poll
      Count : Interfaces.C.int;
   begin
      for Cursor in Self.Map.Iterate loop
         fds (Index).fd := Info_Maps.Key (Cursor);
         fds (Index).events :=
           To_Event_Set (Info_Maps.Element (Cursor).Events);
         fds (Index).revents := 0;
         Index := Index + 1;
      end loop;

      Count := Posix.poll (fds, fds'Length, m_sec);

      if Count > 0 then
         for J in fds'Range loop
            if fds (J).revents /= 0 then
               Count := Count - 1;
               Listener := Self.Map (fds (J).fd).Listener;
               Self.Map.Delete (fds (J).fd);
               Listener.On_Event
                 (Self'Unchecked_Access,
                  fds (J).fd,
                  To_Event_Set (fds (J).revents));
            end if;
         end loop;

      elsif Count < 0 then
         raise Program_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Wait;

end Spawn.Polls.POSIX_Polls;
