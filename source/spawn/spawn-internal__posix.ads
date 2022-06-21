--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Finalization;
with Interfaces.C;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Pipe_Kinds is (Stdin, Stdout, Stderr, Launch);

   type Pipe_Array is array (Pipe_Kinds) of Interfaces.C.int;
   --  File descriptors array

   type Index_Array is array (Pipe_Kinds) of Natural;
   --  Index in poll for each descriptors array

   type Process is new Ada.Finalization.Limited_Controlled with record
      pid   : Interfaces.C.int := 0;
      pipe  : Pipe_Array := (others => 0);
      Index : Index_Array := (others => 0);
   end record;

end Spawn.Internal;
