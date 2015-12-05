(* Example_Docker -- Illustrate Docker support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Lwt.Infix
open Rashell_Docker_j

let map f lst =
  Lwt.return(List.map f lst)

let listcontainers () =
  Rashell_Docker.ps ()
  >>= map string_of_container
  >>= map Yojson.Safe.prettify
  >>= Lwt_list.iter_s Lwt_io.printl

let listimages () =
  Rashell_Docker.images ()
  >>= map string_of_image
  >>= map Yojson.Safe.prettify
  >>= Lwt_list.iter_s Lwt_io.printl

let listtags () =
  Rashell_Docker.tags ()
  >>= Lwt_list.iter_s
    (fun (image, tags) ->
       Lwt_io.printf "%s:  %s\n" image
         (String.concat ", "
            (List.map (fun (container, tag) -> container^":"^tag) tags)))

let shell () =
  Rashell_Docker.(run_shell
                    (options ~tty:true ~argv:[| "/bin/sh" |] ()) "debian:jessie")

let () = Lwt_main.run (listtags())
