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

open Broken
open Rashell_Command
open Lwt.Infix

let assert_process_status ident ?expected_failure f status =
  let broken_f () =
    let t =
      Lwt.catch
        (fun () -> f () >|= fun _ -> WEXITED(0))
        (function
          | Error(_, got, _) -> Lwt.return got
          | exn -> Lwt.fail exn)
    in
    Lwt_main.run t
  in
  assert_equal ident ?expected_failure
    ~printer:pp_print_process_status
    broken_f () status

let assert_utility ident ?expected_failure pred utility =
  let f () =
    let result =
      Lwt_main.run (utility ())
    in
    print_endline result;
    pred result
  in
  assert_true ident ?expected_failure f ()

let assert_test ident ?expected_failure test expected =
  let f () =
    Lwt_main.run (test ())
  in
  assert_equal ident ?expected_failure ~printer:Format.pp_print_bool
    f () expected

let assert_query ident ?expected_failure pred query =
  let f () =
    let result =
      Lwt_main.run (Lwt_stream.to_list (query ()))
    in
    List.iter print_endline result;
    pred result
  in
  assert_true ident ?expected_failure f ()

let assert_filter ident ?expected_failure pred filter input =
  let f () =
    let result =
      Lwt_main.run
        (Lwt_stream.to_list (filter () (Lwt_stream.of_list input)))
    in
    List.iter print_endline result;
    pred result
  in
  assert_true ident ?expected_failure f ()
