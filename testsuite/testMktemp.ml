(* TestMktemp -- Test suite for POSIX utilities

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

open Printf
open Broken
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

let test_tmpfile () =
  let open Lwt_io in
  let validate_perm file =
    if%lwt (Rashell_Posix.(test (Not(Or[
        (* The bitmask must be at most 0o700 *)
        Has_at_least_permission(0o010);
        Has_at_least_permission(0o020);
        Has_at_least_permission(0o040);
        Has_at_least_permission(0o001);
        Has_at_least_permission(0o002);
        Has_at_least_permission(0o004);
      ]))file))
    then
      Lwt.return file
    else
      ksprintf Lwt.fail_with "%s: File has inappropriate permissions: %o"
        file (Unix.stat file).Unix.st_perm
  in
  let write_read file =
    let%lwt () =
      with_file ~mode:Output file (fun chan -> fprintl chan "cookie")
    in
    let%lwt got =
      with_file ~mode:Input file read_line
    in
    if got = "cookie" then
      Lwt.return file
    else
      ksprintf Lwt.fail_with "Cannot read file content: %S" got
  in
  let file_has_been_removed file =
    Lwt.map not
      Rashell_Posix.(test Prune file)
  in
  let prog =
    Rashell_Mktemp.with_tmpfile
      (fun file -> validate_perm file >>= write_read)
    >>= file_has_been_removed
  in
  Lwt_main.run prog

let test_tmpdir () =
  let spec base = [
    (true,  0o700, [ base; "a"]);
    (true,  0o750, [ base; "a"; "b"]);
    (false, 0o600, [ base; "a"; "b"; "x"]);
    (false, 0o640, [ base; "a"; "y" ]);
    (true,  0o700, [ base; "c"]);
    (false, 0o200, [ base; "c"; "z"]);
  ]
  in
  let populate base =
    let%lwt () =
      Toolbox.populate (spec base)
    in
    Lwt.return(base)
  in
  let validate base =
    let test (is_directory, perm, path) =
      Rashell_Posix.(test (And[
          Has_kind(if is_directory then S_DIR else S_REG);
          Has_exact_permission perm
        ]) (List.fold_left Filename.concat "" path))
    in
    let%lwt success =
      Lwt_list.for_all_p test (spec base)
    in
    if success then
      Lwt.return base
    else
      Lwt.fail_with "Could not validate"
  in
  let dir_has_been_removed base =
    if%lwt Rashell_Posix.(test (Has_kind S_DIR)) base then
      ksprintf Lwt.fail_with "%S: Directory still exists." base
    else
      Lwt.return_true
  in
  let prog =
    (Rashell_Mktemp.with_tmpdir
       (fun base -> populate base >>= validate))
    >>= dir_has_been_removed
  in
  Lwt_main.run prog

let () =
  register_suite "mktemp" "Test suite for temporary files and directories" [
    assert_true "tmpfile" test_tmpfile ();
    assert_true "tmpdir" test_tmpdir ();
  ]
