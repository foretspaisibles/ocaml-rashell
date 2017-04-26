(* Rashell_Docker -- Docker support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

open Rashell_Command
open Rashell_Configuration
open Rashell_Docker_t
open Rashell_Docker_j
open Lwt.Infix

module Pool = Set.Make(String)

let failwith fmt =
  Printf.ksprintf
    (fun mesg -> Pervasives.failwith(__MODULE__ ^": "^ mesg))
    fmt

let lwt_failwith fmt =
  Printf.ksprintf
    (fun mesg -> Lwt.fail_with(__MODULE__ ^": "^ mesg))
    fmt

type image_id     = string
type container_id = string

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int
  | Restart_Unless_Stopped

type user = User_ID of int | User_Name of string

type address = string
type ports   = Single of int | Range of int * int

type volume_source =
  | Auto
  | Named of string
  | Path of string

type volume_option =
  | RO
  | Relabel
  | Relabel_Private

type volume_mountpoint = string

type command =
    {
      add_host     : (string * string) list option;
      argv         : string array option;
      cap_add      : string list option;
      cap_drop     : string list option;
      device       : string list option;
      entrypoint   : string option;
      env          : string array option;
      expose       : int list option;
      hostname     : string option;
      image_id     : image_id;
      labels       : (string * string) list option;
      link         : string list option;
      memory       : int option;
      name         : string option;
      net          : string option;
      privileged   : bool option;
      publish      : (int * int) list option;
      publish_gen  : (address option * ports option * ports) list option;
      restart      : restart_policy option;
      tty          : bool option;
      user         : user option;
      volumes_from : container_id list option;
      volumes      : (volume_source * volume_mountpoint * volume_option list) list option;
    }

let error fmt =
  Printf.ksprintf (fun s -> Pervasives.failwith("Rashell_Docker" ^": "^s)) fmt

let tags () =
  let pack lst =
    let images =
      Pool.elements(List.fold_right Pool.add (List.map fst lst) Pool.empty)
    in
    List.map
      (fun x -> (x, List.map snd (List.filter (fun (k,_) -> k = x) lst)))
      images
  in
  let unpack line =
    Scanf.sscanf line "%[^|]|%[^|]|%[^|]"
      (fun id repository tag -> (id, (repository, tag)))
  in
  Lwt_stream.to_list
    (exec_query
       (command ("", [| ac_path_docker; "images";
                        "--format"; "{{.ID}}|{{.Repository}}|{{.Tag}}";
                        "--all=true";
                        "--no-trunc=true"; |])))
  >|= List.map unpack
  >|= List.filter
    (fun (_,(container, tag)) -> container <> "<none>" && tag <> "<none>")
  >|= pack

let _inspect resource of_json_string lst =
  let convert s =
    try Lwt.return(of_json_string s)
    with Ag_oj_run.Error(mesg) | Yojson.Json_error(mesg) ->
      prerr_endline s;
      lwt_failwith "%s: inspect: %S: %s" resource s mesg
  in
  if lst = [] then
    Lwt.return []
  else
    (exec_utility (command ("", (Array.append
                                   [| ac_path_docker; "inspect";|]
                                   (Array.of_list lst)))))
    >>= convert

let _list resource of_json_string () =
  Lwt_stream.to_list
    (exec_query (command ("", [|
         ac_path_docker;
         resource;
         "--all=true";
         "--quiet=true"; |])))
  >>= _inspect resource of_json_string

let ps =
  _list "ps" containers_of_string

let images =
  _list "images" images_of_string

let _exec argv lst =
  if lst = [] then
    Lwt.return_unit
  else
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
  _exec [| "rmi" |] lst

let restart lst =
  _exec [| "restart" |] lst

let pause lst =
  _exec [| "pause" |] lst

let unpause lst =
  _exec [| "unpause" |] lst

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

let string_of_volume_option = function
  | RO -> ":ro"
  | Relabel -> ":z"
  | Relabel_Private -> ":Z"

let docker_args funcname cmd =
  let open Printf in
  maybe_concat [
    (maybe_list
       (fun (host, ip) -> [| "--add-host"; sprintf "%s:%s" host ip |])
       cmd.add_host);
    (maybe_list (fun cap -> [| "--cap-add"; cap |]) cmd.cap_add);
    (maybe_list (fun cap -> [| "--cap-drop"; cap |]) cmd.cap_drop);
    (maybe_list
       (fun binding -> [| "--env"; binding |])
       (match cmd.env with None -> None | Some(arr) -> Some(Array.to_list(arr))));
    (maybe_list (fun dev -> [| "--device"; dev |]) cmd.device);
    (maybe_map (fun cmd -> [| sprintf "--entrypoint=%s" cmd |]) cmd.entrypoint);
    (maybe_list (fun spec -> [| sprintf "--expose=%d" spec |]) cmd.expose);
    (maybe_map (fun spec -> [| sprintf "--hostname=%s" spec |]) cmd.hostname);
    (maybe_list
       (fun (k, v) -> [| sprintf "--label=%s=%s" k v |]) cmd.labels);
    (maybe_list (fun container -> [| sprintf "--link=%s" container |]) cmd.link);
    (maybe_map (fun spec -> [| sprintf "--memory=%dm" spec |]) cmd.memory);
    (maybe_map (fun name -> [| sprintf "--name=%s" name |]) cmd.name);
    (maybe_map (fun net -> [| sprintf "--net=%s" net |]) cmd.net);
    (maybe_list
       (fun (host, container) -> [| sprintf "--publish=%d:%d" host container |])
       cmd.publish);
    (maybe_list
       (fun (addr, host, container) ->
          let string_of_ports = function
            | Single p -> string_of_int p
            | Range (l, h) -> sprintf "%d-%d" l h
          in
            [| sprintf "--publish=%s:%s:%s"
                 (match addr with Some s -> s | None -> "")
                 (match host with Some h -> string_of_ports h | None -> "")
                 (string_of_ports container) |])
       cmd.publish_gen);
    (maybe_map (fun flag -> [| sprintf "--tty=%b" flag |]) cmd.tty);
    (maybe_map
       (function
          | User_ID uid -> [| sprintf "--user=%d" uid |]
          | User_Name name -> [| sprintf "--user=%s" name |])
       cmd.user);
    (maybe_map (fun flag -> [| sprintf "--privileged=%b" flag |]) cmd.privileged);
    (maybe_map
       (function
         | Restart_No -> [| "--restart=no" |]
         | Restart_Always -> [| "--restart=always" |]
         | Restart_Unless_Stopped -> [| "--restart=unless-stopped" |]
         | Restart_On_failure(0) -> [| "--restart=on-failure" |]
         | Restart_On_failure(n) -> [| sprintf "--restart=on-failure:%d" n |])
       cmd.restart);
    (maybe_list
       (fun (src, dst, options) ->

          (* TODO: should we check that
           *   (a) the volume name is correct (alphanumeric
           *       character, followed by [a-z0-9_.-]+)
           *   (b) the source path exists
           *
           * or let docker run fail?
           **)
          if Filename.is_relative dst then
            invalid_arg
              (sprintf
                 "Rashell_Docker.%s: volume destination is not absolute"
                 funcname);

          let vol = match src with
            | Auto -> dst
            | Named volname -> sprintf "%s:%s" volname dst
            | Path src ->
                if Filename.is_relative src then
                  invalid_arg
                    (sprintf
                       "Rashell_Docker.%s: volume source is not absolute"
                       funcname);
                Printf.sprintf "%s:%s" src dst in

          let suffix = String.concat ""
                         (List.map string_of_volume_option options)
          in
            [| sprintf "--volume=%s%s" vol suffix |])
       cmd.volumes);
    (maybe_list
       (fun container -> [| sprintf "--volumes-from=%s" container |])
       cmd.volumes_from);
    Some([| cmd.image_id |]);
    cmd.argv
  ]

let _run funcname exec detach interactive cmd =
  let open Printf in
  let dockerargv =
    Array.append
      [|
        ac_path_docker;
        "run";
        sprintf "--detach=%b" detach;
        sprintf "--interactive=%b" interactive;
        sprintf "--rm=%b" (not detach);
      |]
      (docker_args funcname cmd)
  in
  exec (command ("", dockerargv))

let __run funcname exec detach interactive cmd =
  (* don't let exceptions escape Lwt monad *)
  try
    _run funcname exec detach interactive cmd
  with Invalid_argument _ as exn -> Lwt.fail exn

let run =
  __run "run" (exec_utility ~chomp:true) true false

let run_utility =
  __run "run_utility" exec_utility false false

let run_query =
  _run "run_query" exec_query false false

let run_test =
  __run "run_test" exec_test false false

let run_shell =
  __run "run_shell" exec_shell false true

let create cmd =
  let open Printf in
  let dockerargv =
    Array.append
      [|
        ac_path_docker;
        "create";
      |]
      (docker_args "create" cmd)
  in
  exec_utility ~chomp:true (command ("", dockerargv))

let start containers =
  let open Printf in
  let dockerargv =
    Array.append
      [| ac_path_docker; "start"; |]
      (Array.of_list containers)
  in
  exec_test (command ("", dockerargv))

let command
     ?add_host ?argv ?cap_add ?cap_drop ?device ?entrypoint ?env ?expose
     ?hostname ?labels ?link ?memory ?name ?net ?privileged
     ?publish ?publish_gen ?restart ?tty
     ?user ?volumes_from ?volumes image_id =
  {
    add_host; argv; cap_add; cap_drop; device; entrypoint; env; expose; hostname;
    image_id; labels; link; memory; name; net; privileged;
    publish; publish_gen; restart; tty; user; volumes_from; volumes;
  }
