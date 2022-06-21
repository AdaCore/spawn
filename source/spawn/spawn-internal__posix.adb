--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Spawn.Internal is

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return Standard."=" (Left, Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return Standard."<" (Left, Right);
      end "<";

   end Environments;

end Spawn.Internal;
