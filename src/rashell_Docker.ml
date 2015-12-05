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

module Pool = Set.Make(String)

type image_id     = string
type container_id = string

type restart_policy =
  | Restart_No
  | Restart_Always
  | Restart_On_failure of int

type volume_source =
  | Auto
  | Named of string
  | Path of string

type volume_option =
  | RO
  | Relabel
  | Relabel_private

type volume_mountpoint = string

type options =
    {
      add_host     : (string * string) list option;
      argv         : string array option;
      cap_add      : string list option;
      cap_drop     : string list option;
      device       : string list option;
      entrypoint   : string option;
      env          : string array option;
      expose       : string list option;
      hostname     : string option;
      link         : string list option;
      memory       : int option;
      name         : string option;
      privileged   : bool option;
      publish      : (int * int) list option;
      restart      : restart_policy option;
      tty          : bool option;
      uid          : int option;
      user         : string option;
      volumes_from : container_id list option;
      volumes      : (volume_source * volume_mountpoint * volume_option list) list option;
    }

let ps_keyword = [
  "CONTAINER ID";
  "IMAGE";
  "COMMAND";
  "CREATED";
  "STATUS";
  "PORTS";
  "NAMES";
]

let image_keyword = [
  "REPOSITORY";
  "TAG";
  "IMAGE ID";
  "CREATED";
  "VIRTUAL SIZE";
]

type field = {
  field_name: string;
  field_position: int;
  field_width: int option;
}

let error fmt =
  Printf.ksprintf (fun s -> failwith(__MODULE__ ^": "^s)) fmt

let field_make kwlist header =
  let open Str in
  let pat =
    regexp ("\\(" ^ (String.concat "\\|" kwlist) ^ "\\)\\( *\\)")
  in
  let rec loop ax i =
    match string_match pat header i, i = String.length header with
    | false, false -> error "field_make: Protocol mismatch."
    | false, true -> ax
    | true, _ ->
        let field = {
          field_name = matched_group 1 header;
          field_position = i;
          field_width =
            if matched_group 2 header = "" then
              None
            else
              Some (match_end () - 1 - i)
        }
        in
        loop (field :: ax) (match_end())
  in
  loop [] 0

let field_trim s =
  let open Str in
  global_replace (regexp "\\(^ *\\| *$\\)") "" s

let field_get f s =
  let get () =
    match f.field_width with
    | Some(k) -> String.sub s f.field_position k
    | None -> String.sub s f.field_position
                (String.length s - f.field_position)
  in
  try field_trim (get())
  with _ -> error "field_extract: Protocol mismatch."

let field_extract lst s =
  List.map (fun f -> (f.field_name, field_get f s)) lst

let to_alist name kwlist lst =
  match lst with
  | hd :: tl -> Lwt.return(
      List.map (field_extract (field_make kwlist hd)) tl
    )
  | _ -> Lwt.fail_with(__MODULE__^": "^name^": Protocol mismatch.")

let tags () =
  let triple_of_alist alist =
    let get field = List.assoc field alist in
    try (get "IMAGE ID", (get "REPOSITORY", get "TAG"))
    with Not_found -> failwith(__MODULE__^": images: Protocol mismatch.")
  in
  let pack lst =
    let images =
      Pool.elements(List.fold_right Pool.add (List.map fst lst) Pool.empty)
    in
    List.map
      (fun x -> (x, List.map snd (List.filter (fun (k,_) -> k = x) lst)))
      images
  in
  Lwt_stream.to_list
    (exec_query
       (command ("", [| ac_path_docker; "images"; "--all=true"; "--no-trunc=true"; |])))
  >>= to_alist "images" image_keyword
  >>= Lwt.wrap1 (List.map triple_of_alist)
  >|= List.filter
    (fun (_,(container, tag)) -> container <> "<none>" && tag <> "<none>")
  >|= pack

let _inspect of_json lst =
  let convert s =
    try Lwt.return(of_json s)
    with Ag_oj_run.Error(mesg) | Yojson.Json_error(mesg) ->
      prerr_endline s;
      Printf.ksprintf Lwt.fail_with "%s._inspect: %S: %s"
        __MODULE__ s mesg
  in
  if lst = [] then
    Lwt.return []
  else
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
  | Relabel_private -> ":Z"

let options
     ?add_host ?argv ?cap_add ?cap_drop ?device ?entrypoint ?env ?expose
     ?hostname ?link ?memory ?name ?privileged ?publish ?restart ?tty
     ?uid ?user ?volumes_from ?volumes () =
  {
    add_host; argv; cap_add; cap_drop; device; entrypoint; env; expose; hostname;
    link; memory; name; privileged; publish; restart; tty; uid; user;
    volumes_from; volumes;
  }

let _run funcname exec detach interactive opts image =
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
         opts.add_host);
      (maybe_list (fun cap -> [| "--cap-add"; cap |]) opts.cap_add);
      (maybe_list (fun cap -> [| "--cap-drop"; cap |]) opts.cap_drop);
      (maybe_list
         (fun binding -> [| "--env"; binding |])
         (match opts.env with None -> None | Some(arr) -> Some(Array.to_list(arr))));
      (maybe_list (fun dev -> [| "--device"; dev |]) opts.device);
      (maybe_map (fun cmd -> [| sprintf "--entrypoint=%s" cmd |]) opts.entrypoint);
      (maybe_list (fun spec -> [| sprintf "--expose=%s" spec |]) opts.expose);
      (maybe_map (fun spec -> [| sprintf "--hostname=%s" spec |]) opts.hostname);
      (maybe_list (fun container -> [| sprintf "--link=%s" container |]) opts.link);
      (maybe_map (fun spec -> [| sprintf "--memory=%dm" spec |]) opts.memory);
      (maybe_map (fun name -> [| sprintf "--name=%s" name |]) opts.name);
      (maybe_list
         (fun (host, container) -> [| sprintf "--publish=%d:%d" host container |])
         opts.publish);
      (maybe_map (fun flag -> [| sprintf "--tty=%b" flag |]) opts.tty);
      (maybe_map (fun name -> [| sprintf "--user=%s" name |]) opts.user);
      (maybe_map (fun id -> [| sprintf "--user=%d" id |]) opts.uid);
      (maybe_map (fun flag -> [| sprintf "--privileged=%b" flag |]) opts.privileged);
      (maybe_map
         (function
           | Restart_No -> [| "--restart=no" |]
           | Restart_Always -> [| "--restart=always" |]
           | Restart_On_failure(0) -> [| "--restart=on-failure" |]
           | Restart_On_failure(n) -> [| sprintf "--restart=on-failure:%d" n |])
         opts.restart);
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
         opts.volumes);
      (maybe_list
         (fun container -> [| sprintf "--volumes-from=%s" container |])
         opts.volumes_from);
      Some([| image |]);
      opts.argv
    ]
  in
  exec (command ("", dockerargv))

let __run funcname exec detach interactive opts image =
  (* don't let exceptions escape Lwt monad *)
  try
    _run funcname exec detach interactive opts image
  with Invalid_argument _ as exn -> Lwt.fail exn

let run =
  __run "run" exec_utility true false

let run_utility =
  __run "run_utility" exec_utility false false

let run_query =
  _run "run_query" exec_query false false

let run_test =
  __run "run_test" exec_test false false

let run_shell =
  __run "run_shell" exec_shell false true
