------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
------------------------------------------------------------------------------

with Spawn.Processes.Monitor;
with Spawn.Processes.Windows;

separate (Spawn.Processes)
package body Platform is

   use type Ada.Streams.Stream_Element_Offset;

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

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
   begin
      Windows.Do_Kill_Process (Self);
   end Kill_Process;

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
      procedure On_No_Data;

      ----------------
      -- On_No_Data --
      ----------------

      procedure On_No_Data is
      begin
         Monitor.Enqueue ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdin));
      end On_No_Data;

   begin
      if Self.Status /= Running or Data'Length = 0 then
         Last := Data'First - 1;
         return;
      end if;

      Windows.Do_Write (Self, Data, Last, On_No_Data'Access);
   end Write_Standard_Input;

end Platform;
