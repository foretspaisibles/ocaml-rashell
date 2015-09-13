(* Rashell_Mktemp -- Temporary files and directories

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

let with_tmpfile f =
  exec_utility ~chomp:true (command ("mktemp", [| "/usr/bin/mktemp"; tmptemplate () |]))
  >>= rmwrap ~directory:false f

let with_tmpdir f =
  exec_utility ~chomp:true (command ("mktemp", [| "/usr/bin/mktemp"; "-d"; tmptemplate () |]))
  >>= rmwrap ~directory:true f
