--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Directories;

separate (Spawn.Environments)
function Search_In_Path
  (File : UTF_8_String;
   Path : UTF_8_String) return UTF_8_String
is
   From : Natural := Path'First;
begin
   loop
      declare
         use all type Ada.Directories.File_Kind;

         To : constant Natural := Ada.Strings.Fixed.Index (Path, ":", From);

         Directory : constant UTF_8_String :=
           (if To = 0 then
               Path (From .. Path'Last)
            else
               Path (From .. To - 1));

         Candidate : constant UTF_8_String :=
           Ada.Directories.Compose (Directory, File);
      begin
         if Ada.Directories.Exists (Candidate)
           and then Ada.Directories.Kind (Candidate) = Ordinary_File
         then
            return Ada.Directories.Full_Name (Candidate);
         end if;

         exit when To = 0;

         From := To + 1;
      end;
   end loop;

   return "";
end Search_In_Path;
