--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Spawn.Internal;

procedure Spawn.Processes.Monitor_Loop (Timeout : Integer) is
begin
   Spawn.Internal.Loop_Cycle (Timeout);
end Spawn.Processes.Monitor_Loop;
