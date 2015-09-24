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

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int

val ps : unit -> container list Lwt.t
(** List containers. *)

val images : unit -> image list Lwt.t
(** List images. *)

val stop : string list -> unit Lwt.t
(** Stop a container, given its id. *)

val rm : string list -> unit Lwt.t
(** Remove a container, given its id. *)

val rmi : string list -> unit Lwt.t
(** Remove an image, given its id. *)

val restart : string list -> unit Lwt.t
(** Restart a container, given its id. *)

val run : ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?argv:string array -> string -> string Lwt.t
(** Start a container in detached mode, the returned string is the
    container id. *)

val run_utility : ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?argv:string array -> string -> string Lwt.t
(*** Start a container in attached mode, and return the standard
     output of the program. *)

val run_query : ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?argv:string array -> string -> string Lwt_stream.t
(*** Start a container in attached mode, and return the lines
     written on standard output by the program. *)

val run_test : ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?argv:string array -> string -> bool Lwt.t
(*** Start a container in attached mode, and interpret its return
     status as a predicate. *)

val run_shell : ?add_host:(string * string) list -> ?cap_add:string list -> ?cap_drop:string list -> ?env:string array -> ?device:string list -> ?entrypoint:string -> ?expose:string list -> ?hostname:string -> ?link:string list -> ?memory:int -> ?publish:(int*int)list -> ?tty:bool -> ?user:string -> ?uid:int -> ?privileged:bool -> ?restart:restart_policy -> ?argv:string array -> string -> unit Lwt.t
(*** Start a custom shell in a container. *)
