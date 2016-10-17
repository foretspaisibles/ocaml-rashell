(* Toolbox -- Toolbox for tests

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

open Broken
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

let _make_file (directory, perm, path) =
  let name =
    List.fold_left Filename.concat "" path
  in
  if directory then
    Lwt_unix.mkdir name perm
  else
    Lwt_unix.openfile name [ Unix.O_CREAT ] perm
    >>= Lwt_unix.close

let populate spec =
  Lwt_list.iter_s _make_file spec
