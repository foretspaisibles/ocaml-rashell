(* Rashell_Timestamp -- Timestamp operation

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

type t = float
(** The type of timestamps. *)

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
