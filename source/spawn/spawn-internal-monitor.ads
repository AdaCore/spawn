--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Spawn.Common;

private package Spawn.Internal.Monitor is

   type Command_Kind is
     (Start, Close_Pipe, Watch_Pipe);

   type Command (Kind : Command_Kind := Start) is record
      Process : access Spawn.Internal.Process'Class;
      case Kind is
         when Start =>
            null;
         when Close_Pipe | Watch_Pipe =>
            Pipe : Spawn.Common.Standard_Pipe;
      end case;
   end record;

   procedure Enqueue (Value : Command);

   procedure Loop_Cycle (Timeout : Integer);
   --  Timeout in milliseconds. Dont wait if zero. Wait forever if < 0

end Spawn.Internal.Monitor;
