(* Rashell_Docker -- Docker support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

open Rashell_Docker_t

type image_id     = string
type container_id = string

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int

type user = User_ID of int | User_Name of string

type volume_source =
  | Auto
  | Named of string
  | Path of string

type volume_option =
  | RO
  | Relabel
  | Relabel_Private

type volume_mountpoint = string

val ps : unit -> container list Lwt.t
(** List containers. *)

val images : unit -> image list Lwt.t
(** List images. *)

val tags : unit -> (image_id * (string * string) list) list Lwt.t
(** Examine tags in the local repository.  The result is an
    association list mapping image ids to the (multiple)
    [(repository, tag)] pointing to them. *)

val stop : container_id list -> unit Lwt.t
(** Stop a container, given its id. *)

val rm : container_id list -> unit Lwt.t
(** Remove a container, given its id. *)

val rmi : image_id list -> unit Lwt.t
(** Remove an image, given its id. *)

val restart : container_id list -> unit Lwt.t
(** Restart a container, given its id. *)

type command =
    {
      add_host     : (string * string) list option;
      argv         : string array option;
      cap_add      : string list option;
      cap_drop     : string list option;
      device       : string list option;
      entrypoint   : string option;
      env          : string array option;
      expose       : int list option;
      hostname     : string option;
      image_id     : image_id;
      labels       : (string * string) list option;
      link         : string list option;
      memory       : int option;
      name         : string option;
      net          : string option;
      privileged   : bool option;
      publish      : (int * int) list option;
      restart      : restart_policy option;
      tty          : bool option;
      user         : user option;
      volumes_from : container_id list option;
      volumes      : (volume_source * volume_mountpoint * volume_option list) list option;
    }
(** Options for container execution. *)

val command :
  ?add_host:(string * string) list ->
  ?argv:string array ->
  ?cap_add:string list ->
  ?cap_drop:string list ->
  ?device:string list ->
  ?entrypoint:string ->
  ?env:string array ->
  ?expose:int list ->
  ?hostname:string ->
  ?labels:(string * string) list ->
  ?link:string list ->
  ?memory:int ->
  ?name:string ->
  ?net:string ->
  ?privileged:bool ->
  ?publish:(int*int)list ->
  ?restart:restart_policy ->
  ?tty:bool ->
  ?user:user ->
  ?volumes_from:container_id list ->
  ?volumes:(volume_source * volume_mountpoint * volume_option list) list ->
  image_id ->
  command

val run : command -> container_id Lwt.t
(** Start a container in detached mode, the returned string is the
    container id. *)

val run_utility : command -> string Lwt.t
(** Start a container in attached mode, and return the standard
    output of the program. *)

val run_query : command -> string Lwt_stream.t
(** Start a container in attached mode, and return the lines
    written on standard output by the program. *)

val run_test : command -> bool Lwt.t
(** Start a container in attached mode, and interpret its return
    status as a predicate. *)

val run_shell : command -> unit Lwt.t
(** Start a custom shell in a container. *)
