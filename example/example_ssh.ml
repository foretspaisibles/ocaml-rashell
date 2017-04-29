(* Example_SSH -- Illustrate SSH support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2017 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)
open Lwt.Infix

let uname =
  Rashell_Command.command ("", [| "uname"; "-a" |])

let job1 hostname =
  let cmd =
    Rashell_SSH.command
      (Rashell_SSH.options hostname)
      uname
  in
  Rashell_Command.exec_query cmd
  |> Lwt_stream.iter_s Lwt_io.printl

let job2 hostname =
  Rashell_SSH.utility ~chomp:true (Rashell_SSH.options hostname) "uname -a"
  >>= Lwt_io.printl

let main hostnames =
  Lwt.join (List.map job1 hostnames)

let () = Lwt_main.run (main (List.tl (Array.to_list Sys.argv)))
