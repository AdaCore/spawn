--
--  Copyright (C) 2018-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

procedure Spawn.Processes.Monitor_Loop (Timeout : Integer);
--  Drive process control engine, deliver events, watch child processes, etc.
--
--  Timeout to run in milliseconds. Don't wait if zero. Wait forever if < 0.
--  If there are some events then procedure will return before timeout
--  expires.
--
--  Note: This procedure is NOT thread-safe! In multi-tasking environment
--  create a dedicated task to drive the engine.
--
--  In single task application you should call this to drive process engine.
--
--  In Glib enabled application the engine is integrated it the Glib event
--  loop and this procedure shouldn't be used.
