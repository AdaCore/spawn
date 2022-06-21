--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C.Strings;

package body Spawn.Environments.Internal is

   ---------
   -- Raw --
   ---------

   function Raw
     (Self : Spawn.Environments.Process_Environment'Class)
        return Spawn.Posix.chars_ptr_array
   is
      Index : Positive := 1;
   begin
      return Result : Spawn.Posix.chars_ptr_array
        (1 .. Natural (Self.Map.Length) + 1)
      do
         for J in Self.Map.Iterate loop
            Result (Index) := Interfaces.C.Strings.New_String
              (UTF_8_String_Maps.Key (J) & "=" &
                 UTF_8_String_Maps.Element (J));
            Index := Index + 1;
         end loop;

         Result (Index) := Interfaces.C.Strings.Null_Ptr;
      end return;
   end Raw;

end Spawn.Environments.Internal;
