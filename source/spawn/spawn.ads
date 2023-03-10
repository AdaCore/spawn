--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.UTF_Encoding;
with Interfaces;

package Spawn is
   pragma Pure;

   subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;

   type Process_Status is
    (Not_Running,
     Starting,
     Running);
   --  Current process status.
   --
   --  @value Not_Running  The process has not been started yet or has been
   --  exited/crashed already. Call Start to run it.
   --
   --  @value Starting     The process is launching, but it isn't run yet.
   --
   --  @value Running      The process is running.

   type Process_Exit_Status is (Normal, Crash);
   --  Process exit status
   --  @value Normal   The normal process termination case
   --  @value Crash    The abnormal process termination case

   type Process_Exit_Code is new Interfaces.Unsigned_32;
   --  Exit status reported by the child process on normal exit.
   --  For crash the meaning depends on the OS.

end Spawn;
