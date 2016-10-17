(* Rashell_Git -- Git Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

(** Interface to git.

    These commands all assume that the current working directory is a git
    repository. *)

val topleveldir : ?workdir:string -> unit -> string Lwt.t
(** Show the absolute path of the top-level directory. *)

(** {6 Install, list and run hooks} *)

val hook_list : ?workdir:string -> unit -> string list Lwt.t
(** The list of installed hooks.  This is the list of absolute paths
    to these hooks. *)

val hook_install : ?workdir:string -> ?force:bool -> string -> string -> unit Lwt.t
(** [hook_install name program] install the given [program] as a hook
    called [name], through a symlink. *)

val hook_install_script : ?workdir:string -> ?force:bool -> ?perm:Unix.file_perm -> string -> string -> unit Lwt.t
(** [hook_install_script name script] install a new hook called [name]
    containing the given [script].

    @param perm defaults to [0o700]. *)

val hook_run : ?workdir:string -> ?env:string array -> ?important:bool -> string * (string array) -> bool Lwt.t
(** [hook_run (cmd, argv)] execute the given hook, if it exists.

    The hook is always executed from the top-level directory of the
    repository.

    @param important If set, the absence of the hook is reported as an
    error. *)


(** {6 Operations on branches} *)

val branch_checkout : ?workdir:string -> ?env:string array -> ?create:bool -> ?start:string -> string -> unit Lwt.t
(** [branch_checkout branch] check out the given [branch].

    @param create If set, the branch is created, as for the [-B] flag.
    @param start If set, use this starting point instead of [HEAD]. *)

val branch_list : ?workdir:string -> ?env:string array -> ?merged:bool -> ?all:bool -> unit -> string list Lwt.t
(** [branch_list ()] list local branches.

    @param all If true then all branches are listed, if false then remote branches are listed.
    @param merged If true then only merged branches are listed, if false then only non-merged branches are. *)

val branch_current : ?workdir:string -> ?env:string array -> unit -> string Lwt.t
(** Determine the current branch. If the repository is in detached
    head state, the thread fails. *)

val is_in_detached_head_state : ?workdir:string -> ?env:string array -> unit -> bool Lwt.t
(** Predicate recognising a repository in detached head state. *)

(** {6 Operations on tags} *)

val tag_create : ?workdir:string -> ?env:string array -> ?gpg_sign:bool -> ?message:string -> string -> unit Lwt.t
(** [tag_create tag] create tag. *)

val tag_list : ?workdir:string -> ?env:string array -> unit -> string list Lwt.t
(** [tag_list ()] list tags. *)


(** {6 Common operations} *)

val clone :
  ?workdir:string ->
  ?env:string array ->
  ?template:string ->
  ?bare:bool ->
  ?mirror:bool ->
  ?origin:string ->
  ?branch:string ->
  ?config:(string * string) list ->
  ?depth:int ->
  ?single_branch:bool ->
  ?recursive:bool ->
  ?reference:string ->
  ?dissociate:bool ->
  ?destdir:string ->
  string -> unit Lwt.t
(** Wrapper for the clone command. *)

val add : ?workdir:string -> ?env:string array -> string list -> unit Lwt.t
(** [add paths] update the index using the current content found in
    the working tree, to prepare the content staged for the next
    commit. *)

val commit : ?workdir:string -> ?env:string array -> ?gpg_sign:bool -> string -> unit Lwt.t
(** [commit message] create the next commit. *)

val merge_no_fast_forward :
  ?workdir:string ->
  ?env:string array ->
  ?no_commit:bool ->
  ?squash:bool ->
  ?strategy:string ->
  ?strategy_option:string ->
  ?verify_signatures:bool ->
  ?gpg_sign:bool ->
  ?message:string ->
  string list -> unit Lwt.t
(** [merge_no_fast_forward commits]. *)
