(* Example -- Example

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Lwt.Infix

let list_files dirs =
  Rashell_Posix.(find ~follow:true (And [Has_kind S_REG; Not(Has_suffix ".conf")]) dirs)
  |> Lwt_stream.iter_s Lwt_io.printl

let populate directory =
  let makefile name =
    let c = open_out (Filename.concat directory name) in
    close_out c
  in
  List.iter makefile [ "a"; "b"; "c"; ];
  Lwt.return_unit

let testfind () =
  list_files [ "/etc" ]

let testtmpdir () =
  Rashell_Mktemp.with_tmpdir populate

let () = Lwt_main.run (testfind())
