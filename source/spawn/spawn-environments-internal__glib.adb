--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C;

package body Spawn.Environments.Internal is

   ---------
   -- Raw --
   ---------

   function Raw
     (Self : Process_Environment'Class)
      return Gtkada.Types.Chars_Ptr_Array
   is
      use type Interfaces.C.size_t;

      Index : Interfaces.C.size_t := 1;
   begin
      return Result : Gtkada.Types.Chars_Ptr_Array
        (1 .. Interfaces.C.size_t (Self.Map.Length) + 1)
      do
         for J in Self.Map.Iterate loop
            Result (Index) := Gtkada.Types.New_String
              (UTF_8_String_Maps.Key (J) & "=" &
                 UTF_8_String_Maps.Element (J));
            Index := Index + 1;
         end loop;

         Result (Index) := Gtkada.Types.Null_Ptr;
      end return;
   end Raw;

end Spawn.Environments.Internal;
