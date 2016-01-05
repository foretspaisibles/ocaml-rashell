(* TestCommand -- Test basic command functionalities

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

let test_environment () =
  let open Rashell_Command in
  assert_utility "env"
    (fun s -> s = "Rashell")
    (fun () ->
       exec_utility
         (command
            ~env:[| "RASHELL_COOKIE=Rashell" |]
            ("", [| "sh"; "-c"; "printf '%s' \"${RASHELL_COOKIE}\"" |])))

let () =
  make_suite "command" "Test suite for command funcitonalities"
  |& test_environment ()
  |> register
