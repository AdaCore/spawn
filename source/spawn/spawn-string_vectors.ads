--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Indefinite_Vectors;

package Spawn.String_Vectors is
   pragma Preelaborate;

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => UTF_8_String);

   type UTF_8_String_Vector is new Vectors.Vector with null record;

   Empty_Vector : constant UTF_8_String_Vector :=
     (Vectors.Empty_Vector with null record);

end Spawn.String_Vectors;
