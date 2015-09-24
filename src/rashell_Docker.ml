(* Rashell_Docker -- Docker support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Rashell_Command
open Rashell_Configuration
open Rashell_Docker_t
open Rashell_Docker_j
open Lwt.Infix

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int

let _inspect of_json lst =
  let convert s =
    try Lwt.return(of_json s)
    with Ag_oj_run.Error(mesg) | Yojson.Json_error(mesg) ->
      prerr_endline s;
      Printf.ksprintf Lwt.fail_with "%s._inspect: %S: %s"
        __MODULE__ s mesg
  in
  (exec_utility (command ("", (Array.append
                                 [| ac_path_docker; "inspect";|]
                                 (Array.of_list lst)))))
  >>= convert

let _list resource of_json () =
  Lwt_stream.to_list
    (exec_query (command ("", [|
         ac_path_docker;
         resource;
         "--all=true";
         "--quiet=true"; |])))
  >>= _inspect of_json

let ps =
  _list "ps" containers_of_string

let images =
  _list "images" images_of_string

let _exec argv lst =
  (exec_utility (command ("", Array.concat [
       [| ac_path_docker; |];
       argv;
       Array.of_list lst;
     ])))
  |> Lwt.map ignore

let stop lst =
  _exec [| "stop" |] lst

let rm lst =
  _exec [| "rm" |] lst

let rmi lst =
  _exec [| "lst" |] lst

let restart lst =
  _exec [| "restart" |] lst

let maybe_get = function
  | None -> [| |]
  | Some(array) -> array

let maybe_map f = function
  | None -> None
  | Some(x) -> Some(f x)

let maybe_list f =
  maybe_map (fun lst -> Array.concat (List.map f lst))

let maybe_concat lst =
  Array.concat(List.map maybe_get lst)

let _run exec detach interactive ?add_host ?cap_add ?cap_drop ?env ?device ?entrypoint ?expose ?hostname ?link ?memory ?publish ?tty ?user ?uid ?privileged ?restart ?argv image =
  let open Printf in
  let dockerargv =
    maybe_concat [
      Some([|
          ac_path_docker;
          "run";
          sprintf "--detach=%b" detach;
          sprintf "--interactive=%b" interactive;
          sprintf "--rm=%b" (not detach);
        |]);
      (maybe_list
         (fun (host, ip) -> [| "--add-host"; sprintf "%s:%s" host ip |])
         add_host);
      (maybe_list (fun cap -> [| "--cap-add"; cap |]) cap_add);
      (maybe_list (fun cap -> [| "--cap-drop"; cap |]) cap_drop);
      (maybe_list
         (fun binding -> [| "--env"; binding |])
         (match env with None -> None | Some(arr) -> Some(Array.to_list(arr))));
      (maybe_list (fun dev -> [| "--device"; dev |]) device);
      (maybe_map (fun cmd -> [| sprintf "--entrypoint=%s" cmd |]) entrypoint);
      (maybe_list (fun spec -> [| sprintf "--expose=%s" spec |]) expose);
      (maybe_map (fun spec -> [| sprintf "--hostname=%s" spec |]) hostname);
      (maybe_list (fun container -> [| sprintf "--link=%s" container |]) link);
      (maybe_map (fun spec -> [| sprintf "--memory=%dm" spec |]) memory);
      (maybe_list
         (fun (host, container) -> [| sprintf "--publish=%d:%d" host container |])
         publish);
      (maybe_map (fun flag -> [| sprintf "--tty=%b" flag |]) tty);
      (maybe_map (fun name -> [| sprintf "--user=%s" name |]) user);
      (maybe_map (fun id -> [| sprintf "--user=%d" id |]) uid);
      (maybe_map (fun flag -> [| sprintf "--privileged=%b" flag |]) privileged);
      (maybe_map
         (function
           | Restart_No -> [| "--restart=no" |]
           | Restart_Always -> [| "--restart=always" |]
           | Restart_On_failure(0) -> [| "--restart=on-failure" |]
           | Restart_On_failure(n) -> [| sprintf "--restart=on-failure:%d" n |])
         restart);
      Some([| image |]);
      argv
    ]
  in
  exec (command ("", dockerargv))


let run =
  _run exec_utility true false

let run_utility =
  _run exec_utility false false

let run_query =
  _run exec_query false false

let run_test =
  _run exec_test false false

let run_shell =
  _run exec_shell false true
