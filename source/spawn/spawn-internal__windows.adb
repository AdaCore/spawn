--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Wide_Characters.Unicode;

with Spawn.Internal.Monitor;
with Spawn.Internal.Windows;
with Spawn.Windows_API;

package body Spawn.Internal is
   use type Ada.Streams.Stream_Element_Offset;
   use all type Spawn.Common.Pipe_Kinds;

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) = To_Key (Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) < To_Key (Right);
      end "<";

      ------------
      -- To_Key --
      ------------

      function To_Key (Text : UTF_8_String) return Wide_String is
         Value : Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Text);
      begin
         for Char of Value loop
            Char := Ada.Wide_Characters.Unicode.To_Upper_Case (Char);
         end loop;

         return Value;
      end To_Key;

   end Environments;

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stderr));
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stdin));
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stdout));
   end Close_Standard_Output;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
   begin
      if Self.Status = Running then
         raise Program_Error;
      end if;
   end Finalize;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Self : Process'Class) return String is
      use type Spawn.Windows_API.DWORD;

      Image : constant String := Self.pid.dwProcessId'Image;
   begin
      return (if Self.pid.dwProcessId = 0 then ""
              else Image (2 .. Image'Last));
   end Identifier;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
   begin
      Windows.Do_Kill_Process (Self);
   end Kill_Process;

   ----------------
   -- Loop_Cycle --
   ----------------

   procedure Loop_Cycle (Timeout : Duration)
     renames Spawn.Internal.Monitor.Loop_Cycle;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure On_No_Data;

      procedure On_No_Data is
      begin
         Monitor.Enqueue ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stderr));
      end On_No_Data;
   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Windows.Do_Read (Self, Data, Last, Stderr, On_No_Data'Access);
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure On_No_Data;

      procedure On_No_Data is
      begin
         Monitor.Enqueue ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdout));
      end On_No_Data;
   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Windows.Do_Read (Self, Data, Last, Stdout, On_No_Data'Access);
   end Read_Standard_Output;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Status := Starting;
      Self.Exit_Code := -1;
      Monitor.Enqueue ((Monitor.Start, Self'Unchecked_Access));
   end Start;

   -----------------------
   -- Terminate_Process --
   -----------------------

   procedure Terminate_Process (Self : in out Process'Class) is
   begin
      Windows.Do_Terminate_Process (Self);
   end Terminate_Process;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure On_Has_Data;

      -----------------
      -- On_Has_Data --
      -----------------

      procedure On_Has_Data is
      begin
         Monitor.Enqueue ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdin));
      end On_Has_Data;

   begin
      if Self.Status /= Running or Data'Length = 0 then
         Last := Data'First - 1;
         return;
      end if;

      Windows.Do_Write (Self, Data, Last, On_Has_Data'Access);
   end Write_Standard_Input;

end Spawn.Internal;
