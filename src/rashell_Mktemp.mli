(* Rashell_Mktemp -- Temporary files and directories

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

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
