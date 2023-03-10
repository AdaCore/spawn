--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Unbounded;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Internal;

package body Spawn.Environments.Internal is

   ---------
   -- Raw --
   ---------

   function Raw
     (Self : Process_Environment'Class) return Interfaces.C.wchar_array
   is
      Sum : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   begin
      for J in Self.Map.Iterate loop
         Ada.Strings.Wide_Unbounded.Append
           (Sum,
            Spawn.Internal.Environments.To_Key (UTF_8_String_Maps.Key (J)));

         Ada.Strings.Wide_Unbounded.Append (Sum, "=");

         Ada.Strings.Wide_Unbounded.Append
           (Sum,
            Ada.Strings.UTF_Encoding.Wide_Strings.Decode
              (UTF_8_String_Maps.Element (J)));

         Ada.Strings.Wide_Unbounded.Append (Sum, Wide_Character'Val (0));
      end loop;

      Ada.Strings.Wide_Unbounded.Append (Sum, Wide_Character'Val (0));

      return Interfaces.C.To_C
        (Ada.Strings.Wide_Unbounded.To_Wide_String (Sum));
   end Raw;

end Spawn.Environments.Internal;
