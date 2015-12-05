(* Rashell_Docker -- Docker support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Rashell_Docker_t

type image_id     = string
type container_id = string

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int

type volume_source =
  | Auto
  | Named of string
  | Path of string

type volume_option =
  | RO
  | Relabel
  | Relabel_private

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

type 'a run =
  ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?name:string -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?volumes:(volume_source * volume_mountpoint * volume_option list) list -> ?volumes_from:container_id list -> ?argv:string array -> image_id -> 'a

val run : container_id Lwt.t run
(** Start a container in detached mode, the returned string is the
    container id. *)

val run_utility : string Lwt.t run
(*** Start a container in attached mode, and return the standard
     output of the program. *)

val run_query : string Lwt_stream.t run
(*** Start a container in attached mode, and return the lines
     written on standard output by the program. *)

val run_test : bool Lwt.t run
(*** Start a container in attached mode, and interpret its return
     status as a predicate. *)

val run_shell : unit Lwt.t run
(*** Start a custom shell in a container. *)
