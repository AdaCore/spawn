--
--  Copyright (C) 2018-2026, AdaCore
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
   use type Spawn.Posix.chars_ptr_pointers.Pointer;

   item : Spawn.Posix.chars_ptr_pointers.Pointer :=
     Spawn.Posix.environ;

begin
   if item = null then
      return;
   end if;

   loop
      exit when item.all = Interfaces.C.Strings.Null_Ptr;

      declare
         Text      : constant UTF_8_String :=
           Interfaces.C.Strings.Value (item.all);
         Separator : constant Natural :=
           Ada.Strings.Fixed.Index (Text, "=");
      begin
         exit when Separator = 0;

         Default.Insert
           (Text (Text'First .. Separator - 1),
            Text (Separator + 1 .. Text'Last));
      end;

      Spawn.Posix.chars_ptr_pointers.Increment (item);
   end loop;
end Initialize_Default;
