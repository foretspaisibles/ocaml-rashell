(* TestPosix -- Test suite for POSIX utilities

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

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
  |& assert_process_status "df" (fun () -> df []) (Rashell_Command.WEXITED 0)
  |> register
