--
--  Copyright (C) 2018-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Streams;
with Interfaces.C.Strings;

package Spawn.Posix is

   function open
     (pathname : Interfaces.C.char_array;
      flags    : Interfaces.C.int;
      mode     : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "open";

   function close (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "close";

   function read
     (fd    : Interfaces.C.int;
      buf   : out Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, External_Name => "read";

   function write
     (fd    : Interfaces.C.int;
      buf   : Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, External_Name => "write";

   type Pipe_Ends is (Read_End, Write_End);

   type Fd_Pair is array (Pipe_Ends) of Interfaces.C.int
     with Convention => C;

   function pipe2 (pipefd : out Fd_Pair; flags : Interfaces.C.int)
     return Interfaces.C.int
        with Import, Convention => C, External_Name => "pipe2";

   O_CLOEXEC  : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_CLOEXEC";
   O_NONBLOCK : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_NONBLOCK";
   O_RDWR     : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_RDWR";
   O_NOCTTY   : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_NOCTTY";
   POLLIN     : constant Interfaces.C.unsigned_short
     with Import, Convention => C, External_Name => "SPAWN_POLLIN";
   POLLOUT    : constant Interfaces.C.unsigned_short
     with Import, Convention => C, External_Name => "SPAWN_POLLOUT";
   POLLHUP    : constant Interfaces.C.unsigned_short
     with Import, Convention => C, External_Name => "SPAWN_POLLHUP";
   --
   function fork  return Interfaces.C.int
     with Import, Convention => C, External_Name => "fork";

   function kill
     (pid : Interfaces.C.int;
      sig : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, External_Name => "kill";

   function dup2
     (oldfd : Interfaces.C.int;
      newfd : Interfaces.C.int)
        return Interfaces.C.int
          with Import, Convention => C, External_Name => "dup2";

   function chdir (path : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int
       with Import, Convention => C, External_Name => "chdir";

   type chars_ptr_array is array (Natural range <>) of
     aliased Interfaces.C.Strings.chars_ptr;

   function execve
     (file : Interfaces.C.Strings.chars_ptr;
      argv : chars_ptr_array;
      anvp : chars_ptr_array)
     return Interfaces.C.int
        with Import, Convention => C, External_Name => "execve";

   type pollfd is record
      fd      : Interfaces.C.int;
      events  : Interfaces.C.unsigned_short;
      revents : Interfaces.C.unsigned_short;
   end record with Convention => C;

   type pollfd_array is array (Positive range <>) of pollfd;

   function poll
     (fds     : in out pollfd_array;
      nfds    : Interfaces.C.unsigned_long;
      timeout : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, External_Name => "poll";

   function waitpid
     (pid     : Interfaces.C.int;
      wstatus : access Interfaces.C.unsigned;
      options : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, External_Name => "waitpid";

   WNOHANG : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_WNOHANG";

   function fcntl
     (fd    : Interfaces.C.int;
      cmd   : Interfaces.C.int;
      flags : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "__spawn_fcntli";

   F_SETFD : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_F_SETFD";
   F_SETFL : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_F_SETFL";

   FD_CLOEXEC  : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_FD_CLOEXEC";

   function ioctl
     (fd  : Interfaces.C.int;
      cmd : Interfaces.C.int;
      arg : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "__spawn_ioctli";

   TIOCSCTTY : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_TIOCSCTTY";

   subtype constrained_chars_ptr_array is
     Interfaces.C.Strings.chars_ptr_array (1 .. Interfaces.C.size_t'Last);

   environ : constrained_chars_ptr_array
     with Import, Convention => C, External_Name => "environ";

   --  Errno values
   EINTR  : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_EINTR";
   EAGAIN : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_EAGAIN";

   function posix_openpt
     (flags : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "posix_openpt";

   function grantpt (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "grantpt";

   function unlockpt (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "unlockpt";

   function ptsname_r
     (fd     : Interfaces.C.int;
      buf    : out Interfaces.C.char_array;
      buflen : Interfaces.C.size_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "ptsname_r";

   function setsid return Interfaces.C.int
     with Import, Convention => C, External_Name => "setsid";

end Spawn.Posix;
