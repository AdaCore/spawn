--
--  Copyright (C) 2018-2019, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Windows_API;
with Spawn.Internal;

separate (Spawn.Environments)
procedure Initialize_Default (Default : out Process_Environment) is
   use type Interfaces.C.size_t;
   use type Interfaces.C.wchar_t;
   use type Windows_API.Environment_Block_Access;
   use type Windows_API.BOOL;

   procedure Append (Name, Value : Interfaces.C.wchar_array);

   ------------
   -- Append --
   ------------

   procedure Append (Name, Value : Interfaces.C.wchar_array) is
   begin
      Default.Map.Include
        (Ada.Strings.UTF_Encoding.Wide_Strings.Encode
           (Interfaces.C.To_Ada (Name, False)),
         Ada.Strings.UTF_Encoding.Wide_Strings.Encode
           (Interfaces.C.To_Ada (Value, False)));
   end Append;

   Env   : constant Windows_API.Environment_Block_Access :=
     Windows_API.GetEnvironmentStringsW;
   Equal : Interfaces.C.size_t := 1;
   From  : Interfaces.C.size_t := 1;
   Index : Interfaces.C.size_t := 1;

begin
   if Env /= null then
      loop
         if Env (Index) = Interfaces.C.wide_nul then
            exit when Index = From;
            Append (Env (From .. Equal - 1), Env (Equal + 1 .. Index - 1));
            From := Index + 1;
         elsif Index /= From and then Env (Index) = '=' then
            Equal := Index;
         end if;

         Index := Index + 1;
      end loop;

      if Windows_API.FreeEnvironmentStringsW (Env) = System.Win32.FALSE then
         raise Program_Error;
      end if;
   end if;
end Initialize_Default;
