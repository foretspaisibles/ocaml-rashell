(* TestPosix -- Test suite for POSIX utilities

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
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

let spec base = [
  (true,  0o700, [ base; "a"]);
  (true,  0o750, [ base; "a"; "b"]);
  (false, 0o600, [ base; "a"; "b"; "x"]);
  (false, 0o640, [ base; "a"; "y" ]);
  (true,  0o700, [ base; "c"]);
  (false, 0o200, [ base; "c"; "z"]);
]

let find_fixture =
  let filename = ref "" in
  let cwd = Unix.getcwd () in
  let changeto base =
    filename := base;
    Unix.chdir base;
    Lwt.return base
  in
  let populate base =
    Toolbox.populate (spec base)
  in
  make_fixture
    (fun () ->
       Lwt_main.run
         (Rashell_Mktemp.mktemp ~directory:true ()
          >>= changeto
          >>= populate))
    (fun () ->
       Lwt_main.run
         (Unix.chdir cwd;
          rm ~force:true ~recursive:true [ !filename ]
          |> Lwt_stream.junk_while (fun _ -> true)))

let assert_find id ?expected_failure ?workdir predicate lst =
  assert_equal id ?expected_failure
    ~printer:(fun fft lst -> List.iter (fun x -> Format.fprintf fft " %S" x) lst)
    (fun () -> Lwt_main.run(
         find predicate [ "." ]
         |> Lwt_stream.to_list
         |> Lwt.map (List.filter ((<>) "."))
         |> Lwt.map (List.sort Pervasives.compare)))
    ()
    lst
let () =
  let find_suite =
    make_suite ~fixture:find_fixture "find" "Test suite for find(1)"
    |& assert_find "regular" (Has_kind(S_REG)) [
      "./a/b/x";
      "./a/y";
      "./c/z";
    ]
    |& assert_find "directory" (Has_kind(S_DIR)) [
      "./a";
      "./a/b";
      "./c"
    ]
    |& assert_find "group_can_read" (Has_at_least_permission(0o040)) [
      "./a/b";
      "./a/y"
    ]
    |& assert_find "exact_permission" (Has_exact_permission(0o640)) [
      "./a/y";
    ]
  in
  make_suite "posix" "Test suite for POSIX utilites"
  |& assert_filter "sed_hello"
    (fun lst -> lst = [
         (* Please pardon me, Paul *)
         "I say yes, You say no";
         "I stay stop and You say go, go, go";
         "You say goodbye, and I say hello!";
       ])
    (fun () -> sed_filter "1,2{s/You/They/;s/I/You/;s/They/I/;}")
    [
      "You say yes, I say no";
      "You stay stop and I say go, go, go";
      "You say goodbye, and I say hello!";
    ]
  |* find_suite
  |> register
