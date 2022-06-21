--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Finalization;

with Glib.IOChannel;
with Glib.Main;
with Glib.Spawn;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Pipe_Kinds is (Stdin, Stdout, Stderr);

   type Pipe_Record is record
      FD      : aliased Glib.Gint;
      Channel : Glib.IOChannel.Giochannel;
      Event   : Glib.Main.G_Source_Id;
      --  Pipe is watched if Event /= No_Source_Id
      Watch   : Boolean;
      --  If Read_Standard_*, Write_Standard_Input is called from
      --  Standard_*_Available callback, then set Watch=True to continue
      --  watching of file descriptor.
   end record;

   type Pipe_Array is array (Pipe_Kinds) of Pipe_Record;
   --  File descriptors array

   type Process is tagged;

   type Process_Reference is record
      Self : access Process'Class;
   end record;
   --  A wrapper to pass process pointer to C binding functions

   type Process is new Ada.Finalization.Limited_Controlled with record
      Reference : aliased Process_Reference;
      Event     : Glib.Main.G_Source_Id := 0;
      pid       : aliased Glib.Spawn.GPid := 0;
      pipe      : Pipe_Array :=
        (others => (0, null, Glib.Main.No_Source_Id, False));
   end record;

end Spawn.Internal;
