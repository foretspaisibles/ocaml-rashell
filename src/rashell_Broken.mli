(* Rashell_Broken -- Test cases for Rashell applications

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

val assert_process_status : string -> ?expected_failure:bool ->
  (unit -> 'a Lwt.t) -> Rashell_Command.process_status -> Broken.t
(** [assert_process_status ident f status] create a test case
    running the thread [f ()] and examining its outcome. The test is
    succesful when thread terminates in the {i failed}, holding a
    [Rashell_Command.Error] with the given process status.

    Note that {i filter} and {i query} processes can be combined with
    the [Lwt_stream.to_list] function so that they exhibit a Lwt
    thread. *)

val assert_utility : string -> ?expected_failure:bool ->
  (string -> bool) -> (unit -> string Lwt.t) -> Broken.t
(** [assert_utility ident pred utility] create a test case running the
    [utility] and validating its output with the predicate [pred].
    The test is succesful when the utility terminates succesfully and
    generates an output validated by [pred]. *)

val assert_test : string -> ?expected_failure:bool ->
  (unit -> bool Lwt.t) -> bool -> Broken.t
(** [assert_test ident test expected] create a test case running the
    [test] and comparing its exit status with [expected]. The
    test is succesful when the test terminates succesfully with
    code 0 and [expected] is true or when the utility terminates with
    code 1 and expected is [false]. *)

val assert_query : string -> ?expected_failure:bool ->
  (string list -> bool) -> (unit -> string Lwt_stream.t) -> Broken.t
(** [assert_query ident pred query] create a test case running the
    [query] and validating its output with the predicate [pred].  The test
    is succesful when the query terminates succesfully and generates an
    output validated by [pred]. *)

val assert_filter : string -> ?expected_failure:bool ->
  (string list -> bool) ->
  (unit -> string Lwt_stream.t -> string Lwt_stream.t) ->
  string list -> Broken.t
(** [assert_filter indent pred filter input] create a test case running the
    [filter], feeding it the [input] lines and validating its
    output with the predicate [pred].  The test is succesful when the
    filter terminates succesfully and generates an output validated by
    [pred]. *)
