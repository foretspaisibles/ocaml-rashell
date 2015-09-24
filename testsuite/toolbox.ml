(* Toolbox -- Toolbox for tests

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
