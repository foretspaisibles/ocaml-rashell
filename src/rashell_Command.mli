(* Rashell_Command -- Computations consuming a subprocess

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, with linking exceptions;
   either version 3 of the License, or (at your option) any later
   version. See COPYING file for details.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA. *)

(** Computations consuming a subprocess.

    As discussed by Lowell Jay Arthur in {i Unix shell programming
    (2nd ed.)}, the Unix shell is a brillant tool for quickly
    designing prototypes.  Unluckily, error management is very
    difficult in the shell. Most interfaces to the Unix shell found in
    programming languages share this fragile optimism, which makes
    impossible to write maintainable and resilient programs.

    In this module we define primitives which combine ease of use with
    the ability to write maintainable and resilient programs
    leveraging the full power of Unix. *)


(** {6 Types} *)

(** The type of process status. *)
type process_status = Unix.process_status  =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

(** The type of command descriptions.

    As for [Lwt_process.exec], if the [program] name is the empty string,
    then the first argument will be used. You should specify a name only
    if you do not want the executable to be searched in the PATH. On
    Windows the only way to enable automatic seach in PATH is to pass an
    empty name.

    If [env] supplied, this is the environment of the child
    process, otherwise the environment is inherited.

    If [workdir] supplied, the child process will be started in
    [workdir] instead of the current working directory. *)
type t = {
  program: string;
  argv: string array;
  env: string array option;
  workdir: string option;
}


(** {6 Running computations consuming a subprocess}

    In the cases below, the value computed is either a [Lwt.t] thread or a
    [Lwt_steam.t].

    The environment variable {i RASHELL_DEBUG} can be used to trace
    calls to the programs it enumerates. The list is separated by
    [':']. So for instance, if {i RASHELL_DEBUG} is set to {v grep:sed v}
    then the command descriptions used for the execution of
    ["/usr/bin/sed"] and ["grep"] and other similar combinations will
    be traced on the program standard error channel.  *)

exception Error of t * process_status * string
(** The exception thrown by failed subprocesses.  When the subprocess
consumed by a computation fails, then the thread yielding the result
of the computation or the thread reading from the stream yielding
results of the computation will fail with the given exception.

The string parameter of the exception is the aggregated standard error
output of the failed process. *)


val command : ?workdir:string -> ?env:string array -> string * (string array) -> t
(** [command (program, argv)] prepare a command description with the
    given [program] and argument vector [argv]. *)

(** Execute the given command and return its exit status, the content
    of stdout and of stderr.

    @param chomp Remove the last character if this is a newline
    character (default [false]). *)
val exec_utility : ?chomp:bool -> t -> string Lwt.t

(** Execute the given command and test its exit status. An exit status
    of [0] indicates success of the test, [1] indicates failure of the
    test and other values indicate a general error. *)
val exec_test : t -> bool Lwt.t

(** Execute the given command and return a stream reading the output
    of the command and its exit status with error output. *)
val exec_query : t -> string Lwt_stream.t

(** Execute the given command and return a function mapping streams,
    and its exit status with error output. *)
val exec_filter : t -> string Lwt_stream.t -> string Lwt_stream.t


(** {6 Miscellaneous utilities} *)

val which : ?path:string list -> string -> string option Lwt.t
(** [which name] finds the first matching entry for name in the
    current {i PATH} or the supplied path. *)

val expand_path : string -> string
(** [expand_path name] expand a leading ['~'] and environment
    variables in [name]. If the environment variable {i HOME} is not
    set, then the root of the file-system os used. *)

val chomp : string -> string
(** [chomp s] remove the last character of [s] if it is a newline
    character ['\n']. *)

val is_debugged : string -> bool
(** Predicate recognising componants selected for debugging output. *)

(** {6 Pretty printing}

    The module installs a pretty-printer for the [Error]
    exception. The following functions can be used to pretty-print
    process statuses and command descriptions. *)

val pp_print_process_status : Format.formatter -> process_status -> unit
(** Pretty-printer for process status. *)

val pp_print_command : Format.formatter -> t -> unit
(** Pretty-printer for commands. *)
