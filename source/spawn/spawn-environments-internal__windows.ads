--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;

package Spawn.Environments.Internal is

   function Raw (Self : Process_Environment'Class)
     return Interfaces.C.wchar_array;

end Spawn.Environments.Internal;
