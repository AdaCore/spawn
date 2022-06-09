--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Finalization;

with Glib.Main;
with Glib.Spawn;

with Spawn.Channels;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Pipe_Kinds is (Stdin, Stdout, Stderr);

   type Process is tagged;

   type Process_Reference is record
      Self : access Process'Class;
   end record;
   --  A wrapper to pass process pointer to C binding functions

   type Process is
     abstract new Ada.Finalization.Limited_Controlled with record
      Reference : aliased Process_Reference;
      Channels  : Spawn.Channels.Channels (Process'Unchecked_Access);
      Event     : Glib.Main.G_Source_Id := 0;
      pid       : aliased Glib.Spawn.GPid := 0;
   end record;

   procedure Emit_Stdin_Available (Self : in out Process) is abstract;

   procedure Emit_Stdout_Available (Self : in out Process) is abstract;

   procedure Emit_Stderr_Available (Self : in out Process) is abstract;

   procedure Emit_Error_Occurred
     (Self          : in out Process;
      Process_Error : Integer) is abstract;

end Spawn.Internal;
