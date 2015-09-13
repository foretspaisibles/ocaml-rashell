(* Toolbox -- Toolbox for tests

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
