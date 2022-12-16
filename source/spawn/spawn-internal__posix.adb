--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, "internal GNAT unit");
with System.OS_Interface;
pragma Warnings (On);

with GNAT.OS_Lib;

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

   function Errno return Interfaces.C.int is
     (Interfaces.C.int (GNAT.OS_Lib.Errno));
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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
   begin
      if Self.Status = Running then
         raise Program_Error;
      end if;
   end Finalize;

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

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Process;
      Poll   : Spawn.Polls.Poll_Access;
      Value  : Spawn.Polls.Descriptor;
      Events : Spawn.Polls.Event_Set)
   is
      use all type Spawn.Polls.Event;
      use type Spawn.Polls.Descriptor;
   begin
      if Value = Self.pipe (Launch) then
         if Events (Input) then
            declare
               use type Ada.Streams.Stream_Element_Offset;
               use type Interfaces.C.size_t;

               Count      : Interfaces.C.size_t;
               errno      : Integer := 0;
               Error_Data : Ada.Streams.Stream_Element_Array
                 (1 .. errno'Size / 8)
                 with Import, Convention => Ada, Address => errno'Address;
            begin
               Count := Posix.read (Value, Error_Data, Error_Data'Length);

               if Count = Error_Data'Length then
                  Self.Exit_Code := Process_Exit_Code (errno);
               end if;
            end;
         end if;

         if Events (Close) or Events (Error) then
            if Self.Exit_Code = Process_Exit_Code'Last then
               Self.Status := Running;
               Self.Emit_Started;
               Self.Emit_Stdin_Available;

            else
               Self.Emit_Error_Occurred (Integer (Self.Exit_Code));
            end if;
         end if;

         declare
            Error : constant Interfaces.C.int := Posix.close (Value);
         begin
            if Error /= 0 then
               Self.Emit_Error_Occurred (Integer (Error));
            end if;
         end;
      elsif Value = Self.pipe (Stdin) then
         if Events (Output) then
            Self.Emit_Stdin_Available;
         end if;
      elsif Value = Self.pipe (Stdout) then
         if Events (Input) then
            Self.Emit_Stdout_Available;
         end if;
      elsif Value = Self.pipe (Stderr) then
         if Events (Input) then
            Self.Emit_Stderr_Available;
         end if;
      end if;
   end On_Event;

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

end Spawn.Internal;
