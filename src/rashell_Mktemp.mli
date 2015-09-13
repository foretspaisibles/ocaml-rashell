(* Rashell_Mktemp -- Temporary files and directories

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
