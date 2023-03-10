--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Spawn.Posix;

package Spawn.Environments.Internal is

   function Raw
     (Self : Process_Environment'Class)
      return Spawn.Posix.chars_ptr_array;

end Spawn.Environments.Internal;
