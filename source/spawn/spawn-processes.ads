--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Asynchronous process control API with listener pattern

with Ada.Streams;

with Spawn.Environments;
with Spawn.String_Vectors;
with Spawn.Process_Listeners;

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
   --  Finalization of the Process object in Running state result in wait
   --  for termination of the child process or undefined behavior.
   --
   --  The running process has standard output and standard error streams to
   --  read from and standard input stream to write. Corresponding events
   --  notify the listener when such calls are available.

   type Process_Error is (Failed_To_Start);

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

   procedure Set_Standard_Input_PTY (Self : in out Process'Class);
   --  Configure standard input stream to use pseudo terminal instead
   --  of pipe for communications.

   procedure Set_Standard_Output_PTY (Self : in out Process'Class);
   --  Configure standard output stream to use pseudo terminal instead
   --  of pipe for communications. Note, when both standard output and
   --  standard error streams are configured to use pseudo terminals
   --  they share single underling stream and reported as standard output.

   procedure Set_Standard_Error_PTY (Self : in out Process'Class);
   --  Configure standard error stream to use pseudo terminal instead
   --  of pipe for communications. Note, when both standard output and
   --  standard error streams are configured to use pseudo terminals
   --  they share single underling stream and reported as standard output.

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

   function Identifier (Self : Process'Class) return String;
   --  Return process identifier (PID) image. It returns empty string until
   --  the process starts. After that it returns identifier of the process,
   --  even after process has been finished.

   function Listener (Self : Process'Class)
     return Spawn.Process_Listeners.Process_Listener_Access;

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Spawn.Process_Listeners.Process_Listener_Access)
        with Pre => Self.Status = Not_Running;
   --  Associate a Listener to this event. There may be either zero or one
   --  listener associated to each Process.

   procedure Close_Standard_Input (Self : in out Process'Class);
   --  Close standard input stream to the child process. Application can't
   --  write information to the child process anymore.
   --
   --  Note, when PTY is used for more than one stream, underling connection
   --  will be closed when all streams are closed.
   --
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
   --  Close standard output stream to the child process. Application can't
   --  read information from the child process anymore.
   --
   --  Note, when PTY is used for more than one stream, underling connection
   --  will be closed when all streams are closed.
   --
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
   --
   --  Note, when PTY is used for more than one stream, underling connection
   --  will be closed when all streams are closed.
   --
   --  Do nothing if Self.Status /= Running

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Returns available data received through standard error stream. If no
   --  data was read, the Standard_Error_Available notification will be
   --  emitted later.

   --  For compatibility with older API:
   subtype Process_Listener is Spawn.Process_Listeners.Process_Listener;
   subtype Process_Exit_Code is Spawn.Process_Exit_Code;
   subtype Process_Exit_Status is Spawn.Process_Exit_Status;
   subtype Process_Status is Spawn.Process_Status;

private

   type Process is tagged limited record
      Interal : Spawn.Internal.Process;
   end record;

   function Arguments (Self : Process'Class)
     return Spawn.String_Vectors.UTF_8_String_Vector is
       (Self.Interal.Arguments);

   function Environment (Self : Process'Class)
     return Spawn.Environments.Process_Environment is
       (Self.Interal.Environment);

   function Working_Directory (Self : Process'Class) return UTF_8_String is
     (Self.Interal.Working_Directory);

   function Program (Self : Process'Class) return UTF_8_String is
     (Self.Interal.Program);

   function Status (Self : Process'Class) return Process_Status is
     (Self.Interal.Status);

   function Exit_Status (Self : Process'Class) return Process_Exit_Status is
     (Self.Interal.Exit_Status);
   --  Return the exit status of last process that finishes.

   function Exit_Code (Self : Process'Class) return Process_Exit_Code is
     (Self.Interal.Exit_Code);

   function Listener (Self : Process'Class)
     return Spawn.Process_Listeners.Process_Listener_Access is
       (Self.Interal.Listener);

   function Identifier (Self : Process'Class) return String is
      (Spawn.Internal.Identifier (Self.Interal));

end Spawn.Processes;
