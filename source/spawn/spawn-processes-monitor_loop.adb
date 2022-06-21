--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Spawn.Processes.Monitor;

procedure Spawn.Processes.Monitor_Loop (Timeout : Integer) is
begin
   Spawn.Processes.Monitor.Loop_Cycle (Timeout);
end Spawn.Processes.Monitor_Loop;
