--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  It is Glib's version of the package.

with Ada.Streams;

with Glib.IOChannel;
private with Glib.Main;

with Spawn.Common;
limited with Spawn.Internal;

private package Spawn.Channels is

   type Channels (Process : not null access Spawn.Internal.Process'Class) is
     private;

   type Pipe_Array is array (Spawn.Common.Standard_Pipe) of Glib.Gint;
   --  File descriptors array

   procedure Setup_Channels
     (Self     : in out Channels;
      Use_PTY  : Spawn.Common.Pipe_Flags;
      Child    : out Pipe_Array);

   procedure Close_Child_Descriptors (Self : in out Channels);

   procedure Shutdown_Channels (Self : in out Channels);

   procedure Shutdown_Stdin (Self : in out Channels);

   procedure Shutdown_Stdout (Self : in out Channels);

   procedure Shutdown_Stderr (Self : in out Channels);

   function PTY_Slave (Self : Channels) return Glib.Gint;

   procedure Start_Watch (Self : in out Channels);

   procedure Write_Stdin
     (Self : in out Channels;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Read_Stdout
     (Self : in out Channels;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Read_Stderr
     (Self : in out Channels;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   function Is_Active (Self : Channels) return Boolean;

private

   use type Glib.Gint;

   type Channels (Process : not null access Spawn.Internal.Process'Class) is
      record
         Stdin_Parent  : Glib.IOChannel.Giochannel := null;
         Stdin_Child   : Glib.Gint := -1;
         Stdin_Event   : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
         Stdin_Lock    : Glib.Gboolean := 0;

         Stdout_Parent : Glib.IOChannel.Giochannel := null;
         Stdout_Child  : Glib.Gint := -1;
         Stdout_Event  : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
         Stdout_Lock   : Glib.Gboolean := 0;

         Stderr_Parent : Glib.IOChannel.Giochannel := null;
         Stderr_Child  : Glib.Gint := -1;
         Stderr_Event  : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
         Stderr_Lock   : Glib.Gboolean := 0;

         PTY_Slave     : Glib.Gint := -1;
      end record;

end Spawn.Channels;
