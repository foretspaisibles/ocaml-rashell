(* Rashell_Broken -- Test cases for Rashell applications

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

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
