(* Rashell_Timestamp -- Timestamp operation

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type t = float
(** The type of timestamps. *)

val now : unit -> t
(** A timestamp marking current time. *)

val to_unix : t -> Unix.tm
(** Convert timestamps to Unix time, assumes UTC. *)

val of_unix : Unix.tm -> t
(** Convert Unix time to timestamp, assumes UTC. *)

val offset : float
(** Offset between local time and UTC, the so that [local = utc + offset] *)

val of_string : string -> t
(** Convert textual timestamps to timestamps, assumes UTC.

    @raise Failure if the input string has an incorrect format. *)

val to_string : t -> string
(** Convert timestamps to textual timestamps, assumes UTC. *)

val measure_interval : t -> t -> t
(** [measure_interval a b] is the signed duration of a time interval
    starting at [a] and ending at [b]. *)

val compare : t -> t -> int
(** A comparison function. *)
