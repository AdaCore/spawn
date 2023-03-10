--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C;
with Spawn.Windows_API;

separate (Spawn.Environments)
function Search_In_Path
  (File : UTF_8_String;
   Path : UTF_8_String) return UTF_8_String
is
   use type Spawn.Windows_API.DWORD;

   Raw_Path : Interfaces.C.wchar_array :=
     Interfaces.C.To_C
       (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Path));

   Raw_File : Interfaces.C.wchar_array :=
     Interfaces.C.To_C
       (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (File));

   Raw_Exe : Interfaces.C.wchar_array := Interfaces.C.To_C (".exe");

   Buffer : Interfaces.C.wchar_array (1 .. Spawn.Windows_API.MAX_PATH);

   Length : constant Spawn.Windows_API.DWORD :=
     Spawn.Windows_API.SearchPath
       (lpPath        => Raw_Path (Raw_Path'First)'Unchecked_Access,
        lpFileName    => Raw_File (Raw_File'First)'Unchecked_Access,
        lpExtension   => Raw_Exe (Raw_Exe'First)'Unchecked_Access,
        nBufferLength => Buffer'Length,
        lpBuffer      => Buffer (Buffer'First)'Unchecked_Access,
        lpFilePart    => null);
begin
   return
     (if Length = 0 or else Length > Buffer'Length then ""
      else Ada.Strings.UTF_Encoding.Wide_Strings.Encode
          (Interfaces.C.To_Ada
             (Buffer (1 .. Interfaces.C.size_t (Length)), Trim_Nul => False)));
end Search_In_Path;
