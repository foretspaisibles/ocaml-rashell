(* Rashell_Posix -- General Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)


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
  | Has_exact_permission of int
  | Has_at_least_permission of int
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

val test :
  ?workdir:string ->
  ?env:string array ->
  ?follow:bool ->
  predicate -> string -> bool Lwt.t
(** Test the meta-data of a file.  If the file does not exist, the
    test evaluates to [false]. *)

val cp :
  ?workdir:string ->
  ?env:string array ->
  ?follow:bool ->
  ?force:bool ->
  ?recursive:bool ->
  string list -> string -> string Lwt_stream.t
(** [cp pathlst dest] wraper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html} cp(1)}
    command. *)

val rm :
  ?workdir:string ->
  ?env:string array ->
  ?force:bool ->
  ?recursive:bool ->
  string list -> string Lwt_stream.t
(** [rm pathlst] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/rm.html} rm(1)}
    command. *)

val mv :
  ?workdir:string ->
  ?env:string array ->
  ?force:bool ->
  string list -> string -> string Lwt_stream.t
(** [mv pathlst dest] wrapper of the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mv.html} mv(1)}
    command. *)

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


(** {6 Free disk space} *)

(** The result of a free disk space query. *)
type free_disk_space = {
  df_device: string;
  df_blocks: int;
  df_used: int;
  df_free: int;
  df_capacity: float;
  df_mounted_on: string;
}

val df : string list -> free_disk_space list Lwt.t
(** [df paths] return the available free disk space on the devices for
    the given paths, or for all devices if the empty list is given.  The
    free disk space is computed with ["df -k -P"] as described in
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/df.html}
    df(1)}. *)

val du : string list -> (string * int) list Lwt.t
(** [du paths] query the consumed disk space for the file hierarchies
    rooted at the given paths. Output in kilobytes. *)
