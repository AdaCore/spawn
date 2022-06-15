--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  It is Glib's version of the package.

with Ada.Streams;

with Glib.IOChannel;
private with Glib.Main;

limited with Spawn.Internal;

private package Spawn.Channels is

   type Channels (Process : not null access Spawn.Internal.Process'Class) is
     private;

   type IO_Event_Callback is
     access procedure (Self : in out Spawn.Internal.Process'Class);

   procedure Setup_Channels
     (Self                : in out Channels;
      Standard_Input_PTY  : Boolean;
      Standard_Output_PTY : Boolean);

   procedure Close_Child_Descriptors (Self : in out Channels);

   procedure Shutdown_Channels (Self : in out Channels);

   procedure Shutdown_Stdin (Self : in out Channels);

   procedure Shutdown_Stdout (Self : in out Channels);

   procedure Shutdown_Stderr (Self : in out Channels);

   function Child_Stdin (Self : Channels) return Glib.Gint;

   function Child_Stdout (Self : Channels) return Glib.Gint;

   function Child_Stderr (Self : Channels) return Glib.Gint;

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
