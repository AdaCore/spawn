--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gtkada.Types;

package Spawn.Environments.Internal is

   function Raw
     (Self : Process_Environment'Class)
        return Gtkada.Types.Chars_Ptr_Array;
   --  Each element should be freed by caller using g_free

end Spawn.Environments.Internal;
