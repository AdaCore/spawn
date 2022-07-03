--
--  Copyright (C) 2018-2021, AdaCore
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

package body Spawn.Processes is

   function Errno return Interfaces.C.int;
   --  return errno, number of last error

   ---------------
   -- Arguments --
   ---------------

   function Arguments (Self : Process'Class)
                       return Spawn.String_Vectors.UTF_8_String_Vector is
   begin
      return Self.Arguments;
   end Arguments;

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

   -----------------
   -- Environment --
   -----------------

   function Environment
     (Self : Process'Class)
      return Spawn.Environments.Process_Environment is
   begin
      return Self.Environment;
   end Environment;

   -----------
   -- Errno --
   -----------

   function Errno return Interfaces.C.int is
   begin
      return Interfaces.C.int (GNAT.OS_Lib.Errno);
   end Errno;

   ---------------
   -- Exit_Code --
   ---------------

   function Exit_Code (Self : Process'Class) return Process_Exit_Code is
   begin
      return Self.Exit_Code;
   end Exit_Code;

   -----------------
   -- Exit_Status --
   -----------------

   function Exit_Status (Self : Process'Class) return Process_Exit_Status is
   begin
      return Self.Exit_Status;
   end Exit_Status;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
   begin
      if Self.Status /= Not_Running then
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

   --------------
   -- Listener --
   --------------

   function Listener (Self : Process'Class) return Process_Listener_Access is
   begin
      return Self.Listener;
   end Listener;

   -------------
   -- Program --
   -------------

   function Program (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Program);
   end Program;

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

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Arguments := Arguments;
   end Set_Arguments;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment) is
   begin
      Self.Environment := Environment;
   end Set_Environment;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Process_Listener_Access)
   is
   begin
      Self.Listener := Listener;
   end Set_Listener;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String) is
   begin
      Self.Program := Ada.Strings.Unbounded.To_Unbounded_String (Program);
   end Set_Program;

   ---------------------------
   -- Set_Working_Directory --
   ---------------------------

   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String) is
   begin
      Self.Directory := Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Working_Directory;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Status := Starting;
      Self.Exit_Code := -1;
      Monitor.Enqueue ((Monitor.Start, Self'Unchecked_Access));
   end Start;

   ------------
   -- Status --
   ------------

   function Status (Self : Process'Class) return Process_Status is
   begin
      return Self.Status;
   end Status;

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

   -----------------------
   -- Wait_For_Finished --
   -----------------------

   function Wait_For_Finished
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Wait_For_Finished;

   ---------------------------------------
   -- Wait_For_Standard_Error_Available --
   ---------------------------------------

   function Wait_For_Standard_Error_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Wait_For_Standard_Error_Available;

   ---------------------------------------
   -- Wait_For_Standard_Input_Available --
   ---------------------------------------

   function Wait_For_Standard_Input_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Wait_For_Standard_Input_Available;

   ----------------------------------------
   -- Wait_For_Standard_Output_Available --
   ----------------------------------------

   function Wait_For_Standard_Output_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Wait_For_Standard_Output_Available;

   ----------------------
   -- Wait_For_Started --
   ----------------------

   function Wait_For_Started
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Wait_For_Started;

   -----------------------
   -- Working_Directory --
   -----------------------

   function Working_Directory (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Directory);
   end Working_Directory;

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

end Spawn.Processes;
