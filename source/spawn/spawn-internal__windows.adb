--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Wide_Characters.Unicode;

package body Spawn.Internal is

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) = To_Key (Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) < To_Key (Right);
      end "<";

      ------------
      -- To_Key --
      ------------

      function To_Key (Text : UTF_8_String) return Wide_String is
         Value : Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Text);
      begin
         for Char of Value loop
            Char := Ada.Wide_Characters.Unicode.To_Upper_Case (Char);
         end loop;

         return Value;
      end To_Key;

   end Environments;

end Spawn.Internal;
