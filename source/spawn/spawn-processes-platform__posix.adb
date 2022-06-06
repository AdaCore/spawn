--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Spawn.Processes.Monitor;
with Spawn.Posix;
with GNAT.OS_Lib;

pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with Interfaces.C;

separate (Spawn.Processes)
package body Platform is

   function Errno return Interfaces.C.int;
   --  return errno, number of last error

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

   -----------
   -- Errno --
   -----------

   function Errno return Interfaces.C.int is
   begin
      return Interfaces.C.int (GNAT.OS_Lib.Errno);
   end Errno;

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

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Interfaces.C.size_t;

      Count : Interfaces.C.size_t;
   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Count := Posix.read (Self.pipe (Stderr), Data, Data'Length);

      if Count = Interfaces.C.size_t'Last then
         if Errno in Posix.EAGAIN | Posix.EINTR then
            Last := Data'First - 1;
            Monitor.Enqueue
              ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stderr));
         else
            raise Program_Error with
              "read error: " & GNAT.OS_Lib.Errno_Message;
         end if;
      else
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
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
      use type Interfaces.C.size_t;

      Count : Interfaces.C.size_t;
   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Count := Posix.read (Self.pipe (Stdout), Data, Data'Length);

      if Count = Interfaces.C.size_t'Last then
         if Errno in Posix.EAGAIN | Posix.EINTR then
            Last := Data'First - 1;
            Monitor.Enqueue
              ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdout));
         else
            raise Program_Error with
              "read error: " & GNAT.OS_Lib.Errno_Message;
         end if;
      else
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
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
      use type Interfaces.C.size_t;

      Count : Interfaces.C.size_t;

   begin
      if Self.Status /= Running then
         Last := Data'First - 1;
         return;
      end if;

      Count := Posix.write (Self.pipe (Stdin), Data, Data'Length);
      Last := Data'First - 1;

      if Count = Interfaces.C.size_t'Last then
         if Errno not in Posix.EAGAIN | Posix.EINTR then
            raise Program_Error with
              "write error: " & GNAT.OS_Lib.Errno_Message;
         end if;

      else
         Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
      end if;

      if Count /= Data'Length then
         Monitor.Enqueue
           ((Monitor.Watch_Pipe, Self'Unchecked_Access, Stdin));
      end if;
   end Write_Standard_Input;

end Platform;
