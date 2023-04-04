--
--  Copyright (C) 2018-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Spawn.Windows_API;
with Spawn.Common;

private
package Spawn.Internal.Windows is

   procedure Do_Start_Process
     (Self     : aliased in out Process'Class;
      On_Start : access procedure);

   procedure Do_Terminate_Process (Self : in out Process'Class);

   procedure Do_Kill_Process (Self : in out Process'Class);

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Pipe_Kinds);

   procedure Do_Write
     (Self        : in out Process'Class;
      Data        : Ada.Streams.Stream_Element_Array;
      Last        : out Ada.Streams.Stream_Element_Offset;
      On_Has_Data : access procedure);

   procedure Do_Read
     (Self       : in out Process'Class;
      Data       : out Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      Kind       : Pipe_Kinds;
      On_No_Data : access procedure);

   procedure On_Process_Died (Self : in out Process'Class);

   procedure IO_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context;
      Kind                      : Spawn.Common.Standard_Pipe);
   --  Implementation shared between Standard_[Output/Error]_Callback

   function Error_Message return String;
   --  Return message for the current error retrived with GetLastError.

end Spawn.Internal.Windows;
