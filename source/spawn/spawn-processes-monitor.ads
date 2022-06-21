--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private package Spawn.Processes.Monitor is

   type Command_Kind is
     (Start, Close_Pipe, Watch_Pipe);

   type Command (Kind : Command_Kind := Start) is record
      Process : access Spawn.Processes.Process'Class;
      case Kind is
         when Start =>
            null;
         when Close_Pipe | Watch_Pipe =>
            Pipe : Standard_Pipe;
      end case;
   end record;

   procedure Enqueue (Value : Command);

   procedure Loop_Cycle (Timeout : Integer);
   --  Timeout in milliseconds. Dont wait if zero. Wait forever if < 0

end Spawn.Processes.Monitor;
