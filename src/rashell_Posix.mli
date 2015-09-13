(* Rashell_Posix -- General Commands

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


(** {6 File utilities} *)

(** The type of file types. *)
type file_kind = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

(** File permissions. *)
type file_perm = Unix.file_perm

(** File status *)
type stats = Unix.stats = {
  st_dev: int;
  st_ino: int;
  st_kind: file_kind;
  st_perm: file_perm;
  st_nlink: int;
  st_uid: int;
  st_gid: int;
  st_rdev: int;
  st_size: int;
  st_atime: float;
  st_mtime: float;
  st_ctime: float;
}

type predicate =
  | Prune
  | Has_kind of file_kind
  | Has_suffix of string
  | Is_owned_by_user of int
  | Is_owned_by_group of int
  | Is_newer_than of string
  | Name of string (* Globbing pattern on basename *)
  | And of predicate list
  | Or of predicate list
  | Not of predicate

val find :
  ?workdir:string ->
  ?env:string array ->
  ?follow:bool ->
  ?depthfirst:bool ->
  ?onefilesystem:bool ->
  predicate -> string list -> string Lwt_stream.t
(** [find predicate pathlst] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/find.html} find(1)}
    command. *)

val cp :
  ?workdir:string ->
  ?env:string array ->
  ?follow:bool ->
  ?force:bool ->
  ?recursive:bool ->
  string list -> string -> string Lwt_stream.t
(** [cp pathlst dest] wraper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html} cp(1)}
    command, called with the verbose flag. *)

val rm :
  ?workdir:string ->
  ?env:string array ->
  ?force:bool ->
  ?recursive:bool ->
  string list -> string Lwt_stream.t
(** [rm pathlst] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/rm.html} rm(1)}
    command, called with the verbose flag. *)

val mv :
  ?workdir:string ->
  ?env:string array ->
  ?force:bool ->
  string list -> string -> string Lwt_stream.t
(** [mv pathlst dest] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mv.html} mv(1)}
    command, called with the verbose flag. *)

val ln :
  ?workdir:string ->
  ?env:string array ->
  ?force:bool ->
  ?symbolic:bool ->
  string list -> string -> string Lwt_stream.t
(** [ln pathlst dest] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/ln.html} ln(1)}
    command, called with the verbose flag. *)


(** {6 Sed & Awk} **)

val sed :
  ?workdir:string ->
  ?env:string array ->
  ?echo:bool ->
  string -> string list -> string Lwt_stream.t
(** [sed script files] wrapper of the {i sed(1)} command. *)

val sed_inplace :
  ?workdir:string ->
  ?env:string array ->
  ?suffix:string ->
  string -> string list -> unit Lwt.t
(** [sed_inplace script files] wrapper of the {i sed(1)} command for
    inplace edition. *)

val sed_filter :
  ?workdir:string ->
  ?env:string array ->
  ?echo:bool ->
  string -> string Lwt_stream.t -> string Lwt_stream.t
(** [sed_filter script] wrapper of the {i sed(1)} command for
    being used as a filter. *)

val awk :
  ?workdir:string ->
  ?env:string array ->
  ?fs:string ->
  ?bindings:(string * string) list ->
  string -> string list -> string Lwt_stream.t
(** [awk script files] wrapper of the {i awk(1)} command. *)

val awk_filter :
  ?workdir:string ->
  ?env:string array ->
  ?fs:string ->
  ?bindings:(string * string) list ->
  string -> string Lwt_stream.t -> string Lwt_stream.t
(** [awk script files] wrapper of the {i awk(1)} command. *)
