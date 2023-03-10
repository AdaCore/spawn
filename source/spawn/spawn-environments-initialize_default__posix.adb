--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Fixed;
with Interfaces.C.Strings;

with Spawn.Posix;

separate (Spawn.Environments)
procedure Initialize_Default
  (Default : out Spawn.Environments.Process_Environment)
is
   use type Interfaces.C.Strings.chars_ptr;
begin
   for J in Spawn.Posix.environ'Range loop
      declare
         Item : constant Interfaces.C.Strings.chars_ptr :=
           Spawn.Posix.environ (J);

         Text : constant UTF_8_String :=
           (if Item = Interfaces.C.Strings.Null_Ptr then ""
            else Interfaces.C.Strings.Value (Item));

         Separator : constant Natural :=
           Ada.Strings.Fixed.Index (Text, "=");
      begin
         exit when Separator = 0;

         Default.Insert
           (Text (Text'First .. Separator - 1),
            Text (Separator + 1 .. Text'Last));
      end;
   end loop;
end Initialize_Default;
