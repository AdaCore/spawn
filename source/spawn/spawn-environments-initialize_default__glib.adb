--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Fixed;

with GNAT.Strings;

with Glib.Spawn;

separate (Spawn.Environments)
procedure Initialize_Default
  (Default : out Spawn.Environments.Process_Environment)
is
   List : GNAT.Strings.String_List := Glib.Spawn.Get_Environ;
begin
   for Text of List loop
      declare
         Separator : constant Natural :=
           Ada.Strings.Fixed.Index (Text.all, "=");
      begin
         exit when Separator = 0;

         Default.Insert
           (Text (Text'First .. Separator - 1),
            Text (Separator + 1 .. Text'Last));

         GNAT.Strings.Free (Text);
      end;
   end loop;
end Initialize_Default;
