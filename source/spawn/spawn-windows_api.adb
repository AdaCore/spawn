--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Spawn.Windows_API is

   ----------------
   -- MAKELANGID --
   ----------------

   function MAKELANGID (P : DWORD; S : DWORD) return DWORD is
      use type System.Win32.DWORD;

   begin
      return
        DWORD (Interfaces.Shift_Left (Interfaces.Unsigned_32 (S), 10)) or P;
   end MAKELANGID;

end Spawn.Windows_API;
