--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Wide_Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

pragma Warnings (Off);
with System.OS_Interface;
with System.Win32;
pragma Warnings (On);

with Spawn.Environments.Internal;

package body Spawn.Internal.Windows is
   use all type Spawn.Common.Pipe_Kinds;

   Terminate_Code : constant Windows_API.UINT := 16#F291#;
   --  Arbitrary code to use as exit code for TerminateProcess call.

   package Read_Write_Ex is
     new Windows_API.Generic_Read_Write_Ex (Internal.Context);

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context)
        with Convention => Stdcall;

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context)
        with Convention => Stdcall;

   procedure Append_Escaped_String
     (Command_Line : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Argument     : UTF_8_String);
   --  Append the given argument to a command line such that CommandLineToArgvW
   --  return the argument string unchanged. Arguments in a command line should
   --  be separated by spaces; this subprogram doesn't add these spaces.

   function Internal_Terminate_Process
     (hWnd   : Windows_API.HWND;
      lParam : Windows_API.LPARAM) return Windows_API.BOOL;
   --  Post WM_CLOSE message to the window when given window belongs to given
   --  process. lParam is process identifier in DWORD format.

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

   Pipe_Count : Interfaces.Unsigned_32 := 0;
   --  Counter of pipes created by current process. It is used to construct
   --  unique name of the named pipe.

   ---------------------------
   -- Append_Escaped_String --
   ---------------------------

   procedure Append_Escaped_String
     (Command_Line : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Argument     : UTF_8_String)
   is
      --  Implementation of the subprogram based on Microsoft's blog post
      --  "Everyone quotes command line arguments the wrong way".

      use Ada.Strings.Wide_Unbounded;

      Quotation_Check_Pattern : constant Wide_String :=
        Ada.Characters.Wide_Latin_1.Space
        & Ada.Characters.Wide_Latin_1.Quotation
        & Ada.Characters.Wide_Latin_1.LF
        & Ada.Characters.Wide_Latin_1.HT
        & Ada.Characters.Wide_Latin_1.VT;

      S : constant Wide_String := Ada.Strings.UTF_Encoding.Wide_Strings.Decode
        (Argument);

      J : Natural;  --  Iterator
      N : Natural;  --  Number of sequential backslashes.

   begin
      if S'Length /= 0
        and then Ada.Strings.Wide_Fixed.Index (S, Quotation_Check_Pattern) /= 0
      then
         --  Don't quote unless we actually need to do so - hopefully avoid
         --  problems if programs won't parse quotes properly.

         Append (Command_Line, S);

      else
         Append (Command_Line, '"');
         --  Opening double quotation mark

         J := S'First;

         while J <= S'Last loop
            N := 0;

            while J <= S'Last and then S (J) = '\' loop
               J := J + 1;
               N := N + 1;
            end loop;

            if J > S'Last then
               --  Escape all backslashed, but let the terminating double
               --  quotation mark we add below be interpreted as a
               --  metacharacter.

               Append (Command_Line, (N * 2) * '\');

            elsif S (J) = '"' then
               --  Escape all backslashes and the following double
               --  quotation mark.

               Append (Command_Line, (N * 2 + 1) * '\');
               Append (Command_Line, '"');

            else
               --  Backslashes aren't special here.

               Append (Command_Line, N * '\');
               Append (Command_Line, S (J));
            end if;

            J := J + 1;
         end loop;

         Append (Command_Line, '"');
         --  Closing double quotation mark
      end if;
   end Append_Escaped_String;

   --------------------------------
   -- Internal_Terminate_Process --
   --------------------------------

   function Internal_Terminate_Process
     (hWnd   : Windows_API.HWND;
      lParam : Windows_API.LPARAM) return Windows_API.BOOL
   is
      use type Windows_API.DWORD;

      Process_ID         : constant Windows_API.DWORD :=
        Windows_API.DWORD (lParam);
      Current_Thread_ID  : Windows_API.DWORD with Unreferenced;
      Current_Process_ID : aliased Windows_API.DWORD  := 0;
      Dummy              : Windows_API.BOOL;

   begin
      Current_Thread_ID :=
        Windows_API.GetWindowThreadProcessId (hWnd, Current_Process_ID'Access);

      if Current_Process_ID = Process_ID then
         Dummy := Windows_API.PostMessageW (hWnd, Windows_API.WM_CLOSE, 0, 0);
      end if;

      return System.Win32.TRUE;
   end Internal_Terminate_Process;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Pipe_Kinds)
   is
      use type Windows_API.HANDLE;

      Dummy : Spawn.Windows_API.BOOL;

   begin
      if Self.pipe (Kind).Handle = System.Win32.INVALID_HANDLE_VALUE then
         return;
      end if;

      if Self.pipe (Kind).Waiting_IO then
         Self.pipe (Kind).Close_IO := True;

         Dummy := Windows_API.CancelIo (Self.pipe (Kind).Handle);

      else
         Dummy := System.Win32.CloseHandle (Self.pipe (Kind).Handle);

         Self.pipe (Kind).Handle := System.Win32.INVALID_HANDLE_VALUE;

         if Self.Pending_Finish and then
           (for all Pipe of Self.pipe =>
              Pipe.Handle = System.Win32.INVALID_HANDLE_VALUE)
         then
            Self.Pending_Finish := False;
            Self.Status := Not_Running;
            Self.Emit_Finished (Self.Exit_Status, Self.Exit_Code);
         end if;
      end if;
   end Do_Close_Pipe;

   ---------------------
   -- Do_Kill_Process --
   ---------------------

   procedure Do_Kill_Process (Self : in out Process'Class) is
      Success : Windows_API.BOOL with Unreferenced;

   begin
      Success :=
        Windows_API.TerminateProcess (Self.pid.hProcess, Terminate_Code);
   end Do_Kill_Process;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self       : in out Process'Class;
      Data       : out Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      Kind       : Pipe_Kinds;
      On_No_Data : access procedure)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Count : constant Ada.Streams.Stream_Element_Offset :=
        Self.pipe (Kind).Last;

   begin
      if Count = 0 then
         Last := Data'First - 1;
         On_No_Data.all;
      elsif Count > Data'Length then
         Self.pipe (Kind).Last := Count - Data'Length;
         Last := Data'Last;
         Data := Self.pipe (Kind).Buffer (1 .. Data'Length);

         Self.pipe (Kind).Buffer (1 .. Count - Data'Length) :=
           Self.pipe (Kind).Buffer (Data'Length + 1 .. Count);
      else
         Self.pipe (Kind).Last := 0;
         Last := Data'First + Count - 1;
         Data (Data'First .. Last) := Self.pipe (Kind).Buffer (1 .. Count);
      end if;
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process
     (Self     : aliased in out Process'Class;
      On_Start : access procedure)
   is
      function Make_Command_Line return Interfaces.C.wchar_array;
      function Work_Directory return String;
      function Is_Error (Value : Windows_API.BOOL) return Boolean;
      procedure Request_Read (Kind : Spawn.Common.Standard_Pipe);

      procedure Create_Pipe
        (Parent_Handle : out Windows_API.HANDLE;
         Child_Handle  : out Windows_API.HANDLE;
         Inbound       : Boolean;
         Success       : in out Boolean);

      function Create_Pipes
        (Start : access Windows_API.STARTUPINFOW) return Boolean;

      use type Windows_API.BOOL;
      use type Windows_API.DWORD;
      use type Windows_API.HANDLE;

      -----------------
      -- Create_Pipe --
      -----------------

      procedure Create_Pipe
        (Parent_Handle : out Windows_API.HANDLE;
         Child_Handle  : out Windows_API.HANDLE;
         Inbound       : Boolean;
         Success       : in out Boolean)
      is
         use type Interfaces.Unsigned_32;

         procedure Ignore (Value : System.Win32.BOOL) is null;
         procedure Ignore (Value : System.Win32.DWORD) is null;
         --  Used to ignore returned value of the Windows API functions

         Process_Id_Image : constant String :=
           Spawn.Windows_API.DWORD'Image
             (Spawn.Windows_API.GetCurrentProcessId);

         Mode : constant array (Boolean) of Windows_API.DWORD :=
           (True  => Windows_API.PIPE_ACCESS_INBOUND,
            False => Windows_API.PIPE_ACCESS_OUTBOUND);

         Client_Access : constant array (Boolean) of Windows_API.DWORD :=
           (True  => System.Win32.GENERIC_WRITE,
            False =>
              System.Win32.GENERIC_READ + System.Win32.FILE_WRITE_ATTRIBUTES);

         Inherit_Handle      : aliased System.Win32.SECURITY_ATTRIBUTES :=
           (nLength             => System.Win32.SECURITY_ATTRIBUTES'Size / 8,
            pSecurityDescriptor => System.Null_Address,
            bInheritHandle      => System.Win32.TRUE);
         Dont_Inherit_Handle : aliased System.Win32.SECURITY_ATTRIBUTES :=
           (nLength             => System.Win32.SECURITY_ATTRIBUTES'Size / 8,
            pSecurityDescriptor => System.Null_Address,
            bInheritHandle      => System.Win32.FALSE);

         Pipe_Name : Interfaces.C.Strings.chars_ptr;
         Attempts  : Natural := 1_000;

      begin
         --  Create named pipe and server handle

         loop
            declare
               Pipe_Count_Image : constant String :=
                 Interfaces.Unsigned_32'Image (Pipe_Count);

            begin
               Interfaces.C.Strings.Free (Pipe_Name);
               Pipe_Name :=
                 Interfaces.C.Strings.New_String
                   (Spawn.Windows_API.Pipe_Name_Prefix
                    & "ada-"
                    & Process_Id_Image
                      (Process_Id_Image'First + 1 .. Process_Id_Image'Last)
                    & '-'
                    & Pipe_Count_Image
                      (Pipe_Count_Image'First + 1 .. Pipe_Count_Image'Last));
               --  Each named pipe is created with unique name.

               Pipe_Count := Pipe_Count + 1;

               Parent_Handle := Windows_API.CreateNamedPipeA
                 (lpName               => Pipe_Name,
                  dwOpenMode           =>
                    Mode (Inbound) + Windows_API.FILE_FLAG_OVERLAPPED,
                  dwPipeMode           =>
                    Windows_API.PIPE_TYPE_BYTE
                      + Windows_API.PIPE_WAIT
                      + Windows_API.PIPE_REJECT_REMOTE_CLINETS,
                  nMaxInstances        => 1,
                  nOutBufferSize       => 0,
                  nInBufferSize        => 0,
                  nDefaultTimeOut      => 0,
                  lpSecurityAttributes => Dont_Inherit_Handle'Access);

               exit when Parent_Handle /= System.Win32.INVALID_HANDLE_VALUE;

               declare
                  Error : constant Windows_API.DWORD :=
                    System.Win32.GetLastError;

               begin
                  Attempts := Attempts - 1;

                  if Error /= Windows_API.ERROR_PIPE_BUSY or Attempts = 0 then
                     Self.Emit_Error_Occurred (Integer (Error));

                     Interfaces.C.Strings.Free (Pipe_Name);

                     Child_Handle  := System.Win32.INVALID_HANDLE_VALUE;
                     Parent_Handle := System.Win32.INVALID_HANDLE_VALUE;
                     Success       := False;

                     return;
                  end if;
               end;
            end;
         end loop;

         --  Open named pipe to create client handle

         Child_Handle := Windows_API.CreateFileA
           (lpFileName            => Pipe_Name,
            dwDesiredAccess       => Client_Access (Inbound),
            dwShareMode           => 0,
            lpSecurityAttributes  => Inherit_Handle'Access,
            dwCreationDisposition => System.Win32.OPEN_EXISTING,
            dwFlagsAndAttributes  => Windows_API.FILE_FLAG_OVERLAPPED,
            hTemplateFile         => 0);

         if Child_Handle = System.Win32.INVALID_HANDLE_VALUE then
            Self.Emit_Error_Occurred (Integer (System.Win32.GetLastError));

            Interfaces.C.Strings.Free (Pipe_Name);
            Ignore (System.Win32.CloseHandle (Parent_Handle));

            Child_Handle  := System.Win32.INVALID_HANDLE_VALUE;
            Parent_Handle := System.Win32.INVALID_HANDLE_VALUE;
            Success       := False;

            return;
         end if;

         Interfaces.C.Strings.Free (Pipe_Name);

         --  Wait till connection is in place

         declare
            Overlapped : aliased Windows_API.OVERLAPPED;

         begin
            Overlapped.hEvent :=
              System.OS_Interface.CreateEvent
                (pEventAttributes => null,
                 bManualReset     => System.Win32.TRUE,
                 bInitialState    => System.Win32.FALSE,
                 pName            => Interfaces.C.Strings.Null_Ptr);

            if Windows_API.ConnectNamedPipe (Parent_Handle, Overlapped'Access)
              = System.Win32.FALSE
            then
               declare
                  Error : constant Windows_API.DWORD :=
                    System.Win32.GetLastError;

               begin
                  case Error is
                     when Windows_API.ERROR_PIPE_CONNECTED =>
                        null;

                     when Windows_API.ERROR_IO_PENDING =>
                        Ignore
                          (System.OS_Interface.WaitForSingleObject
                             (Overlapped.hEvent,
                              System.OS_Interface.Wait_Infinite));

                     when others =>
                        Self.Emit_Error_Occurred
                          (Integer (System.Win32.GetLastError));

                        Ignore (System.Win32.CloseHandle (Overlapped.hEvent));
                        Ignore (System.Win32.CloseHandle (Parent_Handle));
                        Ignore (System.Win32.CloseHandle (Child_Handle));

                        Child_Handle  := System.Win32.INVALID_HANDLE_VALUE;
                        Parent_Handle := System.Win32.INVALID_HANDLE_VALUE;
                        Success       := False;

                        return;
                  end case;
               end;
            end if;

            Ignore (System.Win32.CloseHandle (Overlapped.hEvent));
         end;

         Success := True;
      end Create_Pipe;

      ------------------
      -- Create_Pipes --
      ------------------

      function Create_Pipes
        (Start : access Windows_API.STARTUPINFOW) return Boolean
      is
         Child : constant array (Spawn.Common.Standard_Pipe) of
           not null access Windows_API.HANDLE :=
             (Stdin  => Start.hStdInput'Access,
              Stdout => Start.hStdOutput'Access,
              Stderr => Start.hStdError'Access);

         Ok : Boolean := True;
      begin
         for J in Spawn.Common.Standard_Pipe loop
            Self.pipe (J).Process := Self'Unchecked_Access;
            Self.pipe (J).Kind := J;
            Create_Pipe
              (Parent_Handle => Self.pipe (J).Handle,
               Child_Handle  => Child (J).all,
               Inbound       => J /= Stdin,
               Success       => Ok);

            exit when not Ok;
         end loop;

         return Ok;
      end Create_Pipes;

      --------------
      -- If_Error --
      --------------

      function Is_Error (Value : Windows_API.BOOL) return Boolean is
      begin
         if Value = System.Win32.FALSE then
            Self.Emit_Error_Occurred (Integer (System.Win32.GetLastError));
            return True;
         else
            return False;
         end if;
      end Is_Error;

      -----------------------
      -- Make_Command_Line --
      -----------------------

      function Make_Command_Line return Interfaces.C.wchar_array is
         Result : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

      begin
         Append_Escaped_String (Result, Self.Program);

         for Arg of Self.Arguments loop
            Ada.Strings.Wide_Unbounded.Append (Result, ' ');
            Append_Escaped_String (Result, Arg);
         end loop;

         return Interfaces.C.To_C
           (Ada.Strings.Wide_Unbounded.To_Wide_String (Result));
      end Make_Command_Line;

      ------------------
      -- Request_Read --
      ------------------

      procedure Request_Read (Kind : Spawn.Common.Standard_Pipe) is
      begin
         if not Is_Error
           (Read_Write_Ex.ReadFileEx
              (hFile                => Self.pipe (Kind).Handle,
               lpBuffer             => Self.pipe (Kind).Buffer,
               nNumberOfBytesToRead => Self.pipe (Kind).Buffer'Length,
               lpOverlapped         => Self.pipe (Kind)'Access,
               lpCompletionRoutine  => Callback (Kind)))
         then
            Self.pipe (Kind).Waiting_IO := True;
         end if;
      end Request_Read;

      --------------------
      -- Work_Directory --
      --------------------

      function Work_Directory return String is
         Directory : constant String :=
           Ada.Strings.Unbounded.To_String (Self.Directory);
      begin
         return (if Directory = "" then "." else Directory);
      end Work_Directory;

      Start : aliased Windows_API.STARTUPINFOW :=
        (cb      => Windows_API.STARTUPINFOW'Size / 8,
         dwFlags => Windows_API.STARTF_USESTDHANDLES,
         others  => <>);

      Exe : constant Interfaces.C.wchar_array :=
        Interfaces.C.To_C
          (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Self.Program));

      Args : Interfaces.C.wchar_array := Make_Command_Line;

      Env  : constant Interfaces.C.wchar_array :=
        Spawn.Environments.Internal.Raw (Self.Environment);

      Dir  : constant Interfaces.C.wchar_array :=
        Interfaces.C.To_C
          (Ada.Strings.UTF_Encoding.Wide_Strings.Decode
             (Work_Directory));
   begin
      if not Create_Pipes (Start'Access) then
         return;
      end if;

      if Is_Error
        (Windows_API.CreateProcessW
           (lpApplicationName    => Exe,
            lpCommandLine        => Args,
            lpProcessAttributes  => null,
            lpThreadAttributes   => null,
            bInheritHandles      => System.Win32.TRUE,
            dwCreationFlags      => Windows_API.CREATE_NO_WINDOW +
              Windows_API.CREATE_UNICODE_ENVIRONMENT,
            lpEnvironment        => Env,
            lpCurrentDirectory   => Dir,
            lpStartupInfo        => Start'Access,
            lpProcessInformation => Self.pid'Access))
      then
         return;
      end if;

      On_Start.all;

      Self.Status := Running;
      Self.Emit_Started;
      Self.Emit_Stdin_Available;
      Request_Read (Stdout);
      Request_Read (Stderr);
   end Do_Start_Process;

   --------------------------
   -- Do_Terminate_Process --
   --------------------------

   procedure Do_Terminate_Process (Self : in out Process'Class) is
      Success : Windows_API.BOOL with Unreferenced;

   begin
      Success :=
        Windows_API.EnumWindows
          (Internal_Terminate_Process'Access,
           Windows_API.LPARAM (Self.pid.dwProcessId));
      Success :=
         Windows_API.PostThreadMessageW
          (Self.pid.dwThreadId, Windows_API.WM_CLOSE, 0, 0);
   end Do_Terminate_Process;

   --------------
   -- Do_Write --
   --------------

   procedure Do_Write
     (Self        : in out Process'Class;
      Data        : Ada.Streams.Stream_Element_Array;
      Last        : out Ada.Streams.Stream_Element_Offset;
      On_Has_Data : access procedure)
   is
      use type Ada.Streams.Stream_Element_Count;

      Pipe  : Internal.Context renames Self.pipe (Stdin);
      Count : constant Ada.Streams.Stream_Element_Offset := Pipe.Last;
      Min   : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Min
          (Pipe.Buffer'Length, Data'Length);

   begin
      if Count = 0 then
         --  Buffer isn't busy, we can write
         Last := Data'First + Min - 1;
         Pipe.Buffer (1 .. Min) := Data (Data'First .. Last);
         Pipe.Last := Min;

         if Last < Data'Last then
            --  Only part of the data has been buffered, request notification
            --  after completion of the IO operation.

            Pipe.Last := Pipe.Last + Spawn.Internal.Buffer_Size;
         end if;

         On_Has_Data.all;

      elsif Count in Internal.Stream_Element_Buffer'Range then
         --  Buffer is busy, mark stdin as 'send notification'
         Pipe.Last := Pipe.Last + Spawn.Internal.Buffer_Size;
         Last := Data'First - 1;
      else
         --  Buffer is busy and the 'send notification' mark has been set
         Last := Data'First - 1;
      end if;
   end Do_Write;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (dwErrorCode : Spawn.Windows_API.DWORD) return String
   is
      use type Spawn.Windows_API.DWORD;

      Len : Spawn.Windows_API.DWORD;
      Buf : Spawn.Windows_API.LPWSTR;

   begin
      Len :=
        Spawn.Windows_API.FormatMessageW
          (dwFlags      =>
             Spawn.Windows_API.FORMAT_MESSAGE_ALLOCATE_BUFFER
               + Spawn.Windows_API.FORMAT_MESSAGE_FROM_SYSTEM
               + Spawn.Windows_API.FORMAT_MESSAGE_IGNORE_INSERTS,
           lpSource     => System.Null_Address,
           dwMessageId  => dwErrorCode,
           dwLanguageId =>
             Spawn.Windows_API.MAKELANGID
               (Spawn.Windows_API.LANG_NEUTRAL,
                Spawn.Windows_API.SUBLANG_DEFAULT),
           lpBuffer     => Buf,
           nSize        => 0,
           Arguments    => System.Null_Address);

      if Len = 0 then
         return "";
      end if;

      declare
         WB : Wide_String (1 .. Natural (Len)) with Address => Buf.all'Address;

      begin
         return Result : constant String :=
                  Ada.Strings.UTF_Encoding.Wide_Strings.Encode (WB)
         do
            declare
               function To_Address is
                 new Ada.Unchecked_Conversion
                       (Spawn.Windows_API.LPWSTR, System.Address);

               Dummy : System.Address;

            begin
               Dummy := Spawn.Windows_API.LocalFree (To_Address (Buf));
            end;
         end return;
      end;
   end Error_Message;

   -----------------
   -- IO_Callback --
   -----------------

   procedure IO_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context;
      Kind                      : Spawn.Common.Standard_Pipe)
   is
      use type Windows_API.DWORD;
      use type Ada.Streams.Stream_Element_Count;

      Self : Process'Class renames lpOverlapped.Process.all;

      Last : Ada.Streams.Stream_Element_Count := lpOverlapped.Last;

      Transfered : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (dwNumberOfBytesTransfered);

      Completed : constant Boolean :=
        (if Kind = Stdin
         then Transfered in Last | Last - Spawn.Internal.Buffer_Size
         else Transfered > 0);  --  Should be True

   begin
      Self.pipe (Kind).Waiting_IO := False;

      if dwErrorCode = 0 and Completed then
         case Kind is
            when Stdin =>
               lpOverlapped.Last := 0;

               if Last not in lpOverlapped.Buffer'Range then
                  Last := Last - Spawn.Internal.Buffer_Size;
                  Self.Emit_Stdin_Available;
               end if;

            when Stderr =>
               lpOverlapped.Last := Transfered;
               Self.Emit_Stderr_Available;

            when Stdout =>
               lpOverlapped.Last := Transfered;
               Self.Emit_Stdout_Available;
         end case;

         if Self.pipe (Kind).Close_IO then
            --  User's closed pipe, but dwErrorCode isn't set by CancelIo
            Self.pipe (Kind).Close_IO := False;
            Do_Close_Pipe (Self, Kind);
         end if;

      elsif dwErrorCode in 0 | Windows_API.ERROR_OPERATION_ABORTED then
         Do_Close_Pipe (Self, Kind);

      else
         Do_Close_Pipe (Self, Kind);

         case Kind is
            when Stdin =>
               Self.Emit_Standard_Input_Stream_Error
                 (Error_Message (dwErrorCode));

            when Stdout =>
               Self.Emit_Standard_Output_Stream_Error
                 (Error_Message (dwErrorCode));

            when Stderr =>
               Self.Emit_Standard_Error_Stream_Error
                 (Error_Message (dwErrorCode));
         end case;
      end if;
   end IO_Callback;

   ---------------------
   -- On_Process_Died --
   ---------------------

   procedure On_Process_Died (Self : in out Process'Class) is
      use type Windows_API.BOOL;
      use type Windows_API.DWORD;
      use type Windows_API.HANDLE;

      Exit_Code : aliased Windows_API.DWORD := 0;

   begin
      --  Close stdio pipes

      for J in Self.pipe'Range loop
         Do_Close_Pipe (Self, J);
      end loop;

      if Windows_API.GetExitCodeProcess (Self.pid.hProcess, Exit_Code'Access)
           /= System.Win32.FALSE
        and then System.Win32.CloseHandle (Self.pid.hProcess)
                   /= System.Win32.FALSE
        and then System.Win32.CloseHandle (Self.pid.hThread)
                   /= System.Win32.FALSE
      then
         --  Process exit code can be application defined code, Win32 error
         --  code, HRESULT code (including Win32 code or NTSTATUS code
         --  converted into HRESULT), or NTSTATUS code. It is impossible to
         --  recognize used format exactly, thus some heuristic is used to
         --  detect "crash" cases, primary cases when system's HRESULT and
         --  NTSTATUS code reports failure. All custom error codes are
         --  interpreted as normal exit.
         --
         --  Win32 error code occupy two lower bytes and all higher bits are
         --  set to 0.
         --
         --  NTSTATUS upper bits are:
         --   | 31 | 30 | 29 | 28 |
         --   |   Sev   |  C |  N |
         --
         --  HRESULT upper bits are:
         --   | 31 | 30 | 29 | 28 |
         --   |  S |  R |  C |  N |
         --
         --  For both NTSTATUS and HRESULT 'C' is reserved for custom codes.
         --  All these codes interpreted as normal termination.
         --
         --  'N' is '0' for native NTSTATUS, but '1' when NTSTATUS is converted
         --  into HRESULT.
         --
         --  'Sev" is severity of the NTSTATUS. Only 2#11# is interpreted as
         --  crash.
         --
         --  'S" is severity of HRESULT, 2#1# means failure.

         Self.Exit_Status :=
           (if
              Exit_Code = Windows_API.DWORD (Terminate_Code)
         --  Process terminated by call to TerminateProcess

              or else (Exit_Code and 16#F000_0000#) = 16#D000_0000#
         --  NTSTATUS converted into HRESULT with STATUS_SERVERITY_ERROR
         --  (bits 28, 30 and 31 are set to '1', and bit 29 are set to '0')

              or else (Exit_Code and 16#B000_0000#) = 16#8000_0000#
            then Crash
            else Normal);

         Self.Exit_Code := Process_Exit_Code (Exit_Code);

         if (for all Pipe of Self.pipe =>
               Pipe.Handle = System.Win32.INVALID_HANDLE_VALUE)
         then
            Self.Status := Not_Running;
            Self.Emit_Finished (Self.Exit_Status, Self.Exit_Code);
         else
            Self.Pending_Finish := True;
         end if;
      end if;
   end On_Process_Died;

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context) is
   begin
      IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stderr);
   end Standard_Error_Callback;

   ------------------------------
   -- Standard_Output_Callback --
   ------------------------------

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context) is
   begin
      IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdout);
   end Standard_Output_Callback;

end Spawn.Internal.Windows;
