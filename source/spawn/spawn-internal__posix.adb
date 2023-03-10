--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with Spawn.Internal.Monitor;
with Spawn.Posix;

package body Spawn.Internal is
   use all type Spawn.Common.Pipe_Kinds;

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return Standard."=" (Left, Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return Standard."<" (Left, Right);
      end "<";

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
      use type Interfaces.C.int;
      Image : constant String := Self.pid'Image;
   begin
      return (if Self.pid = 0 then "" else Image (2 .. Image'Last));
   end Identifier;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process (Self : in out Process'Class) is
      use type Interfaces.C.int;

      Code : constant Interfaces.C.int :=
        Spawn.Posix.kill
          (Self.pid, Interfaces.C.int (System.OS_Interface.SIGKILL));
   begin
      pragma Assert (Code = 0);
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
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Spawn.Channels.Read (Self.Channels, Stderr, Data, Last);

      if Last = Data'First - 1 then
         Monitor.Enqueue
           ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stderr));
      end if;
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Spawn.Channels.Read (Self.Channels, Stdout, Data, Last);

      if Last = Data'First - 1 then
         Monitor.Enqueue
           ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdout));
      end if;
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
      use type Interfaces.C.int;

      Code : constant Interfaces.C.int :=
        Spawn.Posix.kill
          (Self.pid, Interfaces.C.int (System.OS_Interface.SIGTERM));
   begin
      pragma Assert (Code = 0);
   end Terminate_Process;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Spawn.Channels.Write_Stdin (Self.Channels, Data, Last);

      if Last /= Data'Length then
         Monitor.Enqueue ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdin));
      end if;
   end Write_Standard_Input;

end Spawn.Internal;
