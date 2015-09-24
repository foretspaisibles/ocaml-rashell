(* Example -- Example

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
