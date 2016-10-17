(* TestMktemp -- Test suite for POSIX utilities

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

open Printf
open Broken
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

let test_tmpfile () =
  let open Lwt_io in
  let validate_perm file =
    (Rashell_Posix.(test (Not(Or[
         (* The bitmask must be at most 0o700 *)
         Has_at_least_permission(0o010);
         Has_at_least_permission(0o020);
         Has_at_least_permission(0o040);
         Has_at_least_permission(0o001);
         Has_at_least_permission(0o002);
         Has_at_least_permission(0o004);
       ]))file))
    >>= fun test_successful ->
    if test_successful
    then
      Lwt.return file
    else
      ksprintf Lwt.fail_with "%s: File has inappropriate permissions: %o"
        file (Unix.stat file).Unix.st_perm
  in
  let write_read file =
    with_file ~mode:Output file (fun chan -> fprintl chan "cookie")
    >>= fun () ->
    with_file ~mode:Input file read_line
    >>= fun got ->
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
    Toolbox.populate (spec base)
    >>= fun () ->
    Lwt.return(base)
  in
  let validate base =
    let test (is_directory, perm, path) =
      Rashell_Posix.(test (And[
          Has_kind(if is_directory then S_DIR else S_REG);
          Has_exact_permission perm
        ]) (List.fold_left Filename.concat "" path))
    in
    Lwt_list.for_all_p test (spec base)
    >>= fun success ->
    if success then
      Lwt.return base
    else
      Lwt.fail_with "Could not validate"
  in
  let dir_has_been_removed base =
    Rashell_Posix.(test (Has_kind S_DIR)) base
    >>= fun directory_still_exists ->
    if directory_still_exists then
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
