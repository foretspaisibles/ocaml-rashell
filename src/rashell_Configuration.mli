(* Rashell_Configuration -- Compile time configuration

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

val ac_path_awk : string
(** The compile-time path to the awk program. *)

val ac_path_cp : string
(** The compile-time path to the cp program. *)

val ac_path_find : string
(** The compile-time path to the find program. *)

val ac_path_grep : string
(** The compile-time path to the grep program. *)

val ac_path_id : string
(** The compile-time path to the id program. *)

val ac_path_mv : string
(** The compile-time path to the mv program. *)

val ac_path_rm : string
(** The compile-time path to the rm program. *)

val ac_path_sed : string
(** The compile-time path to the sed program. *)
