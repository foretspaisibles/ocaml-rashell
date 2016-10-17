(* Rashell_Mktemp -- Temporary files and directories

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

open Lwt.Infix
open Rashell_Configuration
open Rashell_Command
open Rashell_Posix

let tmptemplate () =
  let progname () =
    Filename.basename Sys.executable_name
  in
  let tmpdir =
    try Sys.getenv "TMPDIR"
    with Not_found -> "/tmp"
  in
  Filename.concat tmpdir ((progname()) ^ ".XXXXXX")

let rmwrap ~directory f file =
  let remove () =
    if directory && is_debugged "tmpdir" then
      Lwt_io.eprintf "Rashell_Mktemp: tmpdir: %s\n" file
    else
      Lwt_stream.junk_while
        (fun _ -> true)
        (rm ~force:true ~recursive:directory [ file ])
  in
  Lwt.finalize (fun () -> f file) remove

let mktemp ?(directory = false) () =
  let argv =
    if directory then
      [| ac_path_mktemp; "-d"; tmptemplate() |]
    else
      [| ac_path_mktemp; tmptemplate() |]
  in
  exec_utility ~chomp:true (command ("", argv))

let _with_tmpresource directory f =
  mktemp ~directory ()
  >>= rmwrap ~directory f

let with_tmpfile f =
  mktemp ~directory:false ()
  >>= rmwrap ~directory:false f

let with_tmpdir f =
  mktemp ~directory:true ()
  >>= rmwrap ~directory:true f
