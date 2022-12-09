--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;

package Spawn.Process_Listeners is
   pragma Preelaborate;

   type Process_Listener is limited interface;
   --  A process status event listener.
   type Process_Listener_Access is access all Process_Listener'Class;

   procedure Standard_Output_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   procedure Standard_Error_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   procedure Standard_Input_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to write data again.

   procedure Started (Self : in out Process_Listener) is null;
   --  Called when the process is started

   procedure Finished
    (Self        : in out Process_Listener;
     Exit_Status : Process_Exit_Status;
     Exit_Code   : Process_Exit_Code) is null;
   --  Called when the process finishes. Exit_Status is exit status of the
   --  process. On normal exit, Exit_Code is the exit code of the process,
   --  on crash its meaning depends on the operating system. For POSIX systems
   --  it is number of signal when available, on Windows it is process exit
   --  code.

   procedure Error_Occurred
    (Self          : in out Process_Listener;
     Process_Error : Integer) is null;

   procedure Exception_Occurred
     (Self       : in out Process_Listener;
      Occurrence : Ada.Exceptions.Exception_Occurrence) is null;
   --  This will be called when an exception occurred in one of the
   --  callbacks set in place

end Spawn.Process_Listeners;
