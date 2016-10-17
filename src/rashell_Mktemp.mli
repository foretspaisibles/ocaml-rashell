(* Rashell_Mktemp -- Temporary files and directories

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

(** Temporary files and directories.

    These facilities are interface to the {i mktemp(1)} program, which
    is {i not} defined by POSIX but commonly found in Unix systems. *)

val with_tmpfile : (string -> 'a Lwt.t) -> 'a Lwt.t
(** Create a temporary file and apply the given function on its name. *)

val with_tmpdir : (string -> 'a Lwt.t) -> 'a Lwt.t
(** Create a temporary directory and apply the given function on its name. *)

val mktemp : ?directory:bool -> unit -> string Lwt.t
(** Create a temporary file or directory using {i mktemp} and a
    template depemnding on the current program name. *)
