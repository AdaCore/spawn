--
--  Copyright (C) 2018-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with System;
with Interfaces.C.Strings;
with Ada.Streams;
pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

package Spawn.Windows_API is
   pragma Preelaborate;

   subtype HANDLE is System.Win32.HANDLE;
   subtype DWORD is System.Win32.DWORD;
   subtype WORD is System.Win32.WORD;
   subtype BOOL is System.Win32.BOOL;
   subtype WCHAR is Interfaces.C.wchar_t;
   subtype LPSTR is Interfaces.C.Strings.chars_ptr;
   subtype LPBYTE is LPSTR;
   type LPWSTR is access all WCHAR;
   type UINT is new Interfaces.C.unsigned;
   type LONG_PTR is mod 2 ** Standard'Address_Size;
   type UINT_PTR is mod 2 ** Standard'Address_Size;
   subtype LPARAM is LONG_PTR;
   subtype WPARAM is UINT_PTR;
   type HWND is new System.Win32.PVOID;

   type STARTUPINFOW is record
      cb              : DWORD := 0;
      lpReserved      : LPWSTR;
      lpDesktop       : LPWSTR;
      lpTitle         : LPWSTR;
      dwX             : DWORD := 0;
      dwY             : DWORD := 0;
      dwXSize         : DWORD := 0;
      dwYSize         : DWORD := 0;
      dwXCountChars   : DWORD := 0;
      dwYCountChars   : DWORD := 0;
      dwFillAttribute : DWORD := 0;
      dwFlags         : DWORD := 0;
      wShowWindow     : WORD := 0;
      cbReserved2     : WORD := 0;
      lpReserved2     : LPBYTE;
      hStdInput       : aliased HANDLE := 0;
      hStdOutput      : aliased HANDLE := 0;
      hStdError       : aliased HANDLE := 0;
   end record
     with Convention => C;

   STARTF_USESTDHANDLES : constant DWORD := 16#00000100#;
   --  The hStdInput, hStdOutput, and hStdError members contain additional
   --  information.
   --
   --  If this flag is specified when calling one of the process creation
   --  functions, the handles must be inheritable and the function's
   --  bInheritHandles parameter must be set to TRUE.

   type PROCESS_INFORMATION is record
      hProcess    : HANDLE;
      hThread     : HANDLE;
      dwProcessId : DWORD;
      dwThreadId  : DWORD;
   end record
     with Convention => C;

   CREATE_NO_WINDOW : constant DWORD := 16#08000000#;
   --  The process is a console application that is being run without a console
   --  window. Therefore, the console handle for the application is not set.

   CREATE_UNICODE_ENVIRONMENT : constant DWORD := 16#00000400#;
   --  If this flag is set, the environment block pointed to by lpEnvironment
   --  uses Unicode characters. Otherwise, the environment block uses ANSI
   --  characters.

   function CreateProcessW
     (lpApplicationName    : Interfaces.C.wchar_array;
      lpCommandLine        : in out Interfaces.C.wchar_array;
      lpProcessAttributes  : access System.Win32.SECURITY_ATTRIBUTES;
      lpThreadAttributes   : access System.Win32.SECURITY_ATTRIBUTES;
      bInheritHandles      : BOOL;
      dwCreationFlags      : DWORD;
      lpEnvironment        : Interfaces.C.wchar_array;
      lpCurrentDirectory   : Interfaces.C.wchar_array;
      lpStartupInfo        : access STARTUPINFOW;
      lpProcessInformation : access PROCESS_INFORMATION)
      return BOOL
        with Import, Convention => Stdcall, External_Name => "CreateProcessW";

   function TerminateProcess
     (hProcess  : HANDLE;
      uExitCode : UINT) return BOOL
      with Import, Convention => Stdcall, External_Name => "TerminateProcess";

   type OVERLAPPED is record
      Internal     : System.Address := System.Null_Address;
      InternalHigh : System.Address := System.Null_Address;
      Offset       : DWORD := 0;
      OffsetHigh   : DWORD := 0;
      hEvent       : HANDLE := 0;
   end record;

   generic
      type CUSTOM_OVERLAPPED is limited private;
   package Generic_Read_Write_Ex is

      type Callback is access procedure
        (dwErrorCode               : DWORD;
         dwNumberOfBytesTransfered : DWORD;
         lpOverlapped              : access CUSTOM_OVERLAPPED)
           with Convention => Stdcall;

      function ReadFileEx
        (hFile                : HANDLE;
         lpBuffer             : out Ada.Streams.Stream_Element_Array;
         nNumberOfBytesToRead : DWORD;
         lpOverlapped         : access CUSTOM_OVERLAPPED;
         lpCompletionRoutine  : Callback)
           return BOOL
             with Import, Convention => Stdcall, External_Name => "ReadFileEx";

      function WriteFileEx
        (hFile                 : HANDLE;
         lpBuffer              : Ada.Streams.Stream_Element_Array;
         nNumberOfBytesToWrite : DWORD;
         lpOverlapped          : access CUSTOM_OVERLAPPED;
         lpCompletionRoutine   : Callback)
           return BOOL
             with Import, Convention => Stdcall,
                  External_Name => "WriteFileEx";

   end Generic_Read_Write_Ex;

   function CancelIo (hFile : HANDLE) return BOOL
     with Import, Convention => Stdcall, External_Name => "CancelIo";

   function CreateNamedPipeA
     (lpName                 : Interfaces.C.Strings.chars_ptr;
      dwOpenMode             : DWORD;
      dwPipeMode             : DWORD;
      nMaxInstances          : DWORD;
      nOutBufferSize         : DWORD;
      nInBufferSize          : DWORD;
      nDefaultTimeOut        : DWORD;
      lpSecurityAttributes   : access System.Win32.SECURITY_ATTRIBUTES)
      return HANDLE
        with Import, Convention => Stdcall,
             External_Name => "CreateNamedPipeA";

   PIPE_ACCESS_INBOUND  : constant DWORD := 16#1#;
   --  The flow of data in the pipe goes from client to server only. This mode
   --  gives the server the equivalent of GENERIC_READ access to the pipe. The
   --  client must specify GENERIC_WRITE access when connecting to the pipe.
   PIPE_ACCESS_OUTBOUND : constant DWORD := 16#2#;
   --  The flow of data in the pipe goes from server to client only. This mode
   --  gives the server the equivalent of GENERIC_WRITE access to the pipe. The
   --  client must specify GENERIC_READ access when connecting to the pipe.
   FILE_FLAG_OVERLAPPED       : constant DWORD := 16#40000000#;
   PIPE_TYPE_BYTE             : constant DWORD := 0;
   PIPE_WAIT                  : constant DWORD := 0;
   PIPE_REJECT_REMOTE_CLINETS : constant DWORD := 16#0000_0008#;
   PIPE_UNLIMITED_INSTANCES   : constant DWORD := 255;

   Pipe_Name_Prefix : constant String := "\\.\pipe\";

   function ConnectNamedPipe
     (hNamedPipe   : HANDLE;
      lpOverlapped : access OVERLAPPED) return BOOL
     with Import, Convention => Stdcall, External_Name => "ConnectNamedPipe";

   function CreateFileA
     (lpFileName            : Interfaces.C.Strings.chars_ptr;
      dwDesiredAccess       : DWORD;
      dwShareMode           : DWORD;
      lpSecurityAttributes  : access System.Win32.SECURITY_ATTRIBUTES;
      dwCreationDisposition : DWORD;
      dwFlagsAndAttributes  : DWORD;
      hTemplateFile         : HANDLE) return HANDLE
     with Import, Convention => Stdcall, External_Name => "CreateFileA";

   function CreateEventW
     (lpSecurityAttributes : access System.Win32.SECURITY_ATTRIBUTES;
      bManualReset         : BOOL;
      bInitialState        : BOOL;
      lpName               : LPWSTR)
      return HANDLE
        with Import, Convention => Stdcall, External_Name => "CreateEventW";

   function SetEvent (hEvent : HANDLE) return BOOL
     with Import, Convention => Stdcall, External_Name => "SetEvent";

   type HANDLE_Array is array (Positive range <>) of HANDLE;

   function WaitForMultipleObjectsEx
     (nCount         : DWORD;
      lpHandles      : HANDLE_Array;
      bWaitAll       : BOOL;
      dwMilliseconds : DWORD;
      bAlertable     : BOOL)
        return DWORD
          with Import, Convention => Stdcall,
               External_Name => "WaitForMultipleObjectsEx";

   MAXIMUM_WAIT_OBJECTS : constant := 64;
   WAIT_IO_COMPLETION   : constant DWORD := 16#C0#;
   WAIT_TIMEOUT         : constant DWORD := 16#102#;

   function GetExitCodeProcess
     (hProcess    : HANDLE;
      lpExitCode  : access DWORD)
        return BOOL
          with Import, Convention => Stdcall,
               External_Name => "GetExitCodeProcess";

   subtype Environment_Block is
     Interfaces.C.wchar_array (1 .. Interfaces.C.size_t'Last);
   type Environment_Block_Access is access all Environment_Block;

   function GetEnvironmentStringsW return Environment_Block_Access
     with Import, Convention => Stdcall,
          External_Name => "GetEnvironmentStringsW";

   function FreeEnvironmentStringsW
     (lpszEnvironmentBlock : Environment_Block_Access)
       return BOOL
         with Import, Convention => Stdcall,
              External_Name => "FreeEnvironmentStringsW";

   function Get_Handle
     (fd : Interfaces.C.int)
       return HANDLE
         with Import, Convention => Stdcall,
              External_Name => "_get_osfhandle";

   ERROR_PIPE_BUSY         : constant DWORD := 231;
   ERROR_PIPE_CONNECTED    : constant DWORD := 535;
   ERROR_OPERATION_ABORTED : constant DWORD := 995;
   --  he I/O operation has been aborted because of either a thread exit or an
   --  application request.
   ERROR_IO_PENDING        : constant DWORD := 997;

   function SearchPath
     (lpPath         : LPWSTR;
      lpFileName     : LPWSTR;
      lpExtension    : LPWSTR;
      nBufferLength  : DWORD;
      lpBuffer       : LPWSTR;
      lpFilePart     : access LPWSTR)
        return DWORD
          with Import, Convention => Stdcall,
               External_Name => "SearchPathW";
   --  Searches for a specified file in a specified path

   MAX_PATH : constant := 32767;
   --  Max file name length. Legacy MAX_PATH was 256

   function GetCurrentProcessId return DWORD
     with Import, Convention => Stdcall,
          External_Name => "GetCurrentProcessId";

   type WNDENUMPROC is access function
     (HWnd  : Windows_API.HWND;
      Param : LPARAM) return BOOL;

   function EnumWindows
     (lpEnumFunc : WNDENUMPROC;
      lParam     : Windows_API.LPARAM) return BOOL
     with Import, Convention => Stdcall, External_Name => "EnumWindows";

   function GetWindowThreadProcessId
     (hWnd          : Windows_API.HWND;
      lpdwProcessId : access DWORD) return DWORD
     with Import,
          Convention    => Stdcall,
          External_Name => "GetWindowThreadProcessId";

   WM_CLOSE : constant := 16#0010#;

   function PostMessageW
     (hWnd   : Windows_API.HWND;
      Msg    : UINT;
      wParam : Windows_API.WPARAM;
      lParam : Windows_API.LPARAM) return BOOL
     with Import, Convention => Stdcall, External_Name => "PostMessageW";

   function PostThreadMessageW
     (idThread : DWORD;
      Msg      : UINT;
      wParam   : Windows_API.WPARAM;
      lParam   : Windows_API.LPARAM) return BOOL
     with Import, Convention => Stdcall, External_Name => "PostThreadMessageW";

end Spawn.Windows_API;
