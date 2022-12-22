--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Windows_API;
with Spawn.Internal.Windows;

package body Spawn.Internal.Monitor is
   use all type Spawn.Common.Pipe_Kinds;

   subtype Context is Internal.Context;
   subtype Stream_Element_Buffer is Internal.Stream_Element_Buffer;

   type Process_Access is access all Process'Class;

   procedure Start_Process (Self : access Process'Class);

   package Command_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (Command);

   package Command_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Command_Queue_Interfaces);

   package Read_Write_Ex is
     new Windows_API.Generic_Read_Write_Ex (Context);

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Do_Watch_Pipe
     (Process : not null Process_Access;
      Kind    : Spawn.Common.Standard_Pipe);

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

   Queue : Command_Queues.Queue;

   package Poll is
      procedure Wake_Up;

      procedure Add_Process (Process : Process_Access);
      procedure Wait_Process_Death (Timeout : Duration);
   end Poll;

   package body Poll is
      package Info_Vectors is new Ada.Containers.Vectors
        (Positive, Process_Access);
      type pollfd_array_access is access all Windows_API.HANDLE_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Windows_API.HANDLE_Array, pollfd_array_access);

      List  : Info_Vectors.Vector;
      fds   : pollfd_array_access;
      Last  : Natural := 0;
      wake  : Windows_API.HANDLE := 0;

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process (Process : Process_Access) is
      begin
         if fds = null then
            --  Make event for wake up poll and initialize fds
            wake := Windows_API.CreateEventW
              (lpSecurityAttributes => null,
               bManualReset         => System.Win32.FALSE,
               bInitialState        => System.Win32.FALSE,
               lpName               => null);
            fds := new Windows_API.HANDLE_Array (1 .. 5);
            fds (1) := wake;
            List.Append (null); --  Dummy value
            Last := 1;
         elsif fds'Length < Last + 1 then
            --  Grow fds by factor of 1.5
            declare
               Old : pollfd_array_access := fds;
            begin
               fds := new Windows_API.HANDLE_Array (1 .. fds'Last * 3 / 2);
               fds (Old'Range) := Old.all;
               Free (Old);
            end;
         end if;

         Last := Last + 1;
         Process.Index := Last;

         fds (Last) := Process.pid.hProcess;

         if List.Last_Index < Last then
            List.Append (Process);
         else
            List (Last) := (Process);
         end if;
      end Add_Process;

      ------------------------
      -- Wait_Process_Death --
      ------------------------

      procedure Wait_Process_Death (Timeout : Duration) is
         use type Windows_API.DWORD;

         procedure Swap (Left, Right : in out Windows_API.HANDLE);

         procedure Swap (Left, Right : in out Windows_API.HANDLE) is
            Copy : constant Windows_API.HANDLE := Left;
         begin
            Left := Right;
            Right := Copy;
         end Swap;

         Result    : Windows_API.DWORD;
         Index     : Integer;
         Process   : Process_Access;

      begin
         loop
            Result := Windows_API.WaitForMultipleObjectsEx
              (nCount         => Windows_API.DWORD (Last),
               lpHandles      => fds.all,
               bWaitAll       => System.Win32.FALSE,
               dwMilliseconds => Windows_API.DWORD (Timeout * 1000.0),
               bAlertable     => System.Win32.TRUE);

            if Result /= Windows_API.WAIT_IO_COMPLETION then
               exit;
            end if;
         end loop;

         Index := Integer (Result) + 1;

         if Result = Windows_API.WAIT_TIMEOUT then
            return;
         elsif Index = 1 then
            --  Wake up event triggered
            return;
         elsif Index <= Last then
            --  Some process died, drop it from fds and List
            Process := List (Index);
            Swap (fds (Index), fds (Last));
            List.Swap (Index, Last);
            List.Delete_Last;
            Last := Last - 1;
            Windows.On_Process_Died (Process.all);
         else
            raise Program_Error with "WaitForMultipleObjectsEx FAILED";
         end if;
      end Wait_Process_Death;

      -------------
      -- Wake_Up --
      -------------

      procedure Wake_Up is
         use type Windows_API.BOOL;
         use type Windows_API.HANDLE;
         Result : Windows_API.BOOL;
      begin
         if wake /= 0 then  --  If wake has been created
            Result := Windows_API.SetEvent (wake);
            pragma Assert (Result /= System.Win32.FALSE);
         end if;
      end Wake_Up;
   end Poll;

   -------------------
   -- Do_Watch_Pipe --
   -------------------

   procedure Do_Watch_Pipe
     (Process : not null Process_Access;
      Kind    : Spawn.Common.Standard_Pipe)
   is
      use type Ada.Streams.Stream_Element_Count;
      use type Windows_API.BOOL;
      Ok : Windows_API.BOOL;
      Last : Ada.Streams.Stream_Element_Count;
   begin
      case Kind is
         when Stdout | Stderr =>
            Ok := Read_Write_Ex.ReadFileEx
              (hFile                => Process.pipe (Kind).Handle,
               lpBuffer             => Process.pipe (Kind).Buffer,
               nNumberOfBytesToRead => Process.pipe (Kind).Buffer'Length,
               lpOverlapped         => Process.pipe (Kind)'Access,
               lpCompletionRoutine  => Callback (Kind));

            if Ok = System.Win32.FALSE then
               Process.Listener.Error_Occurred
                 (Integer (System.Win32.GetLastError));
            else
               Process.pipe (Kind).Waiting_IO := True;
            end if;
         when Stdin =>
            Last := Process.pipe (Kind).Last;

            if Last not in Stream_Element_Buffer'Range then
               Last := Last - Stream_Element_Buffer'Last;
            end if;

            Ok := Read_Write_Ex.WriteFileEx
              (hFile                 => Process.pipe (Kind).Handle,
               lpBuffer              => Process.pipe (Kind).Buffer,
               nNumberOfBytesToWrite => Windows_API.DWORD (Last),
               lpOverlapped          => Process.pipe (Kind)'Access,
               lpCompletionRoutine   => Standard_Input_Callback'Access);

            if Ok = System.Win32.FALSE then
               Process.Listener.Error_Occurred
                 (Integer (System.Win32.GetLastError));
            else
               Process.pipe (Kind).Waiting_IO := True;
            end if;
      end case;
   end Do_Watch_Pipe;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Value : Command) is
   begin
      Queue.Enqueue (Value);
      Poll.Wake_Up;
   end Enqueue;

   ----------------
   -- Loop_Cycle --
   ----------------

   procedure Loop_Cycle (Timeout : Duration) is
      use type Ada.Containers.Count_Type;
      Command : Monitor.Command;
   begin
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Command);

         case Command.Kind is
            when Start =>
               Start_Process (Command.Process);
            when Close_Pipe =>
               Windows.Do_Close_Pipe (Command.Process.all, Command.Pipe);
            when Watch_Pipe =>
               Do_Watch_Pipe (Command.Process, Command.Pipe);
         end case;
      end loop;

      Poll.Wait_Process_Death (Timeout);
   end Loop_Cycle;

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stderr);
   end Standard_Error_Callback;

   ------------------------------
   -- Standard_Output_Callback --
   ------------------------------

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdout);
   end Standard_Output_Callback;

   -----------------------------
   -- Standard_Input_Callback --
   -----------------------------

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdin);
   end Standard_Input_Callback;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process (Self : access Process'Class) is
      procedure On_Start;

      procedure On_Start is
      begin
         Poll.Add_Process (Process_Access (Self));
      end On_Start;
   begin
      Windows.Do_Start_Process (Self.all, On_Start'Access);
   end Start_Process;

end Spawn.Internal.Monitor;
