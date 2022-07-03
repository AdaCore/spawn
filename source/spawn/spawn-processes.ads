--
--  Copyright (C) 2018-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Asynchronous process control API with listener pattern

with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Interfaces;

with Spawn.Environments;
with Spawn.String_Vectors;

private with Spawn.Internal;

package Spawn.Processes is

   type Process is tagged limited private;
   --  The Process represents a running, exited, crashed or not-yet-running
   --  process.
   --
   --  The just created process has Not_Running status.
   --  In this status Program, Arguments, Environment, Working_Directory and
   --  Listener could be configured on the process. The Start call changes
   --  the process status to Starting. Since then no configuration allowed.
   --  If the OS is able to run corresponding program then the status becomes
   --  Running and Started, Standard_Input_Available events are triggered on
   --  the listener. Otherwise status becomes Not_Running and Error_Occurred
   --  event is signaled.
   --  A running process keeps Running state till it crashes or exit normally,
   --  then state becomes Not_Running and Finished event is triggered.
   --
   --  Note: Make sure to keep Process object alive while it has Running
   --  state. The suggested pattern is to keep it in the listener object.
   --
   --  The running process has standard output and standard error streams to
   --  read from and standard input stream to write. Corresponding events
   --  notify the listener when such calls are available.

   type Process_Exit_Status is (Normal, Crash);
   --  Process exit status
   --  @value Normal   The normal process termination case
   --  @value Crash    The abnormal process termination case

   type Process_Exit_Code is new Interfaces.Unsigned_32;
   --  Exit status reported by the child process on normal exit.
   --  For crash the meaning depends on the OS.

   type Process_Listener is limited interface;
   --  A process status event listener.
   type Process_Listener_Access is access all Process_Listener'Class;

   procedure Standard_Output_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   procedure Standard_Error_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to read data again.

   procedure Standard_Input_Available
    (Self : in out Process_Listener) is null;
   --  Called once when it's possible to write data again.

   procedure Started (Self : in out Process_Listener) is null;
   --  Called when the process is started

   procedure Finished
    (Self        : in out Process_Listener;
     Exit_Status : Process_Exit_Status;
     Exit_Code   : Process_Exit_Code) is null;
   --  Called when the process finishes. Exit_Status is exit status of the
   --  process. On normal exit, Exit_Code is the exit code of the process,
   --  on crash its meaning depends on the operating system. For POSIX systems
   --  it is number of signal when available, on Windows it is process exit
   --  code.

   procedure Error_Occurred
    (Self          : in out Process_Listener;
     Process_Error : Integer) is null;

   procedure Exception_Occurred
     (Self       : in out Process_Listener;
      Occurrence : Ada.Exceptions.Exception_Occurrence) is null;
   --  This will be called when an exception occurred in one of the
   --  callbacks set in place

   type Process_Error is (Failed_To_Start);

   type Process_Status is
    (Not_Running,
     Starting,
     Running);
   --  Current process status.
   --
   --  @value Not_Running  The process has not been started yet or has been
   --  exited/crashed already. Call Start to run it.
   --
   --  @value Starting     The process is launching, but it isn't run yet.
   --
   --  @value Running      The process is running.

   function Arguments (Self : Process'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector;
   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector)
        with Pre => Self.Status = Not_Running;
   --  Command line arguments

   function Environment (Self : Process'Class)
     return Spawn.Environments.Process_Environment;

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment)
        with Pre => Self.Status = Not_Running;
   --  Process environment

   function Working_Directory (Self : Process'Class) return UTF_8_String;
   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String)
        with Pre => Self.Status = Not_Running;
   --  Working directory

   function Program (Self : Process'Class) return UTF_8_String;
   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String)
        with Pre => Self.Status = Not_Running;
   --  Executables name. Note that this should be a resolved path,
   --  this API does not look for executables on the PATH.

   procedure Start (Self : in out Process'Class)
     with Pre => Self.Status = Not_Running;

   function Status (Self : Process'Class) return Process_Status;

   function Exit_Status (Self : Process'Class) return Process_Exit_Status
     with Pre => Self.Status = Not_Running;
   --  Return the exit status of last process that finishes.

   function Exit_Code (Self : Process'Class) return Process_Exit_Code
     with Pre => Self.Status = Not_Running;
   --  Return the exit code of last process that finishes when exit status is
   --  Normal, or signal number (on POSIX systems) or exit code (on Windows).

   procedure Terminate_Process (Self : in out Process'Class);
   --  Ask process to exit. Process can ignore this request.
   --
   --  On Windows, WM_CLOSE message are post, and on POSIX, the SIGTERM signal
   --  is sent.

   procedure Kill_Process (Self : in out Process'Class);
   --  Kill current process. Process will exit immediately.
   --
   --  On Windows, TerminateProcess() is called, and on POSIX, the SIGKILL
   --  signal is sent.

   function Listener (Self : Process'Class) return Process_Listener_Access;
   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Process_Listener_Access)
        with Pre => Self.Status = Not_Running;
   --  Associate a Listener to this event. There may be either zero or one
   --  listener associated to each Process.

   procedure Close_Standard_Input (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Do nothing if Self.Status /= Running. Last is set to index of the last
   --  element to be written. If Last < Data'Last it means incomplete
   --  operation, Standard_Input_Available notification will be called once
   --  operation can be continued. Application is responsible to call this
   --  subprogram again for remaining data.

   procedure Close_Standard_Output (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Returns available data received through standard output stream. If no
   --  data was read, the Standard_Output_Available notification will be
   --  emitted later.

   procedure Close_Standard_Error (Self : in out Process'Class);
   --  Do nothing if Self.Status /= Running

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Returns available data received through standard error stream. If no
   --  data was read, the Standard_Error_Available notification will be
   --  emitted later.

   function Wait_For_Started
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Block until process has started or until Timeout have passed. Return
   --  True when process has been started successfully.
   --
   --  Started subprogram of the listener is called before exit from this
   --  subprogram.

   function Wait_For_Finished
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Block until process has been finished ot until Timeout have passed.
   --  Return True when process has finished.
   --
   --  Finished subprogram of the listener is called before exit from this
   --  subprogram.

   function Wait_For_Standard_Input_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Block until standard input is available for write.
   --
   --  Standard_Input_Available subprogram of the listener is called before
   --  exit from this subprogram.

   function Wait_For_Standard_Output_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Block until standard output has data available for read.
   --
   --  Standard_Output_Available subprogram of the listener is called before
   --  exit from this subprogram.

   function Wait_For_Standard_Error_Available
     (Self    : in out Process'Class;
      Timeout : Duration := Duration'Last) return Boolean;
   --  Block until standard error has data available for read.
   --
   --  Standard_Error_Available subprogram of the listener is called before
   --  exit from this subprogram.

private

   use all type Internal.Pipe_Kinds;
   subtype Pipe_Kinds is Internal.Pipe_Kinds;
   subtype Standard_Pipe is Pipe_Kinds range Stdin .. Stderr;

   type Process is new Spawn.Internal.Process with record
      Arguments   : Spawn.String_Vectors.UTF_8_String_Vector;
      Environment : Spawn.Environments.Process_Environment :=
        Spawn.Environments.System_Environment;
      Exit_Status : Process_Exit_Status := Normal;
      Exit_Code   : Process_Exit_Code := Process_Exit_Code'Last;
      Status      : Process_Status := Not_Running;

      Listener    : Process_Listener_Access;
      --  The associated listener. Note: this may be null.

      Program     : Ada.Strings.Unbounded.Unbounded_String;
      Directory   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Finalize (Self : in out Process);

end Spawn.Processes;
