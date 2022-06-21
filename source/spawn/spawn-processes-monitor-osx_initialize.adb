--
--  Copyright (C) 2018-2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  This is an OSX implementation. For some reason, we are unable to catch
--  SIGCHLD signal until it's enabled for the environment task.
--

with Ada.Interrupts.Names;
with Interfaces.C;

pragma Warnings (Off);
with System.OS_Interface;
pragma Warnings (Off);

separate (Spawn.Processes.Monitor)
procedure Initialize is
   Ignore : Interfaces.C.int;
   Value  : aliased System.OS_Interface.struct_sigaction :=
     (sa_flags    => 0,
      others      => <>);
begin
   --  Reset sigaction to call a null procedure
   Ignore := System.OS_Interface.sigemptyset
     (Value.sa_mask'Unrestricted_Access);
   --  Set dummy procedure as the handler
   Value.sa_handler := Dummy'Address;

   --  Assign the custom handler to SIGCHLD signal
   Ignore := System.OS_Interface.sigaction
     (System.OS_Interface.Signal (Ada.Interrupts.Names.SIGCHLD),
      Value'Unchecked_Access,
      null);
end Initialize;
