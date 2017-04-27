(* Rashell_SSH -- SSH support

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2017 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)

type options = {
  hostname:string;
  config_file: string option; (* -F *)
  stdin_null: bool; (* -n *)
  fork_after_authentication: bool; (* -f *)
  forward_x11: bool; (* -Y *)
  identity_file: string option; (* -i *)
  local_forward: string option; (* -L *)
  remote_forward: string option; (* -R *)
  dynamic_forward: string option; (* -D *)
  user: string option; (* -l *)
  no_shell: bool; (* -N *)
  request_tty: bool option; (* -t *)
  port: string option; (* -p *)
  options: string list; (* -o *)
  workdir: string option; (* client workdir *)
  env: string array option; (* client env *)
}

type t = options * Rashell_Command.t

let options
    ?config_file
    ?(stdin_null = false)
    ?(fork_after_authentication = false)
    ?(forward_x11 = false)
    ?identity_file
    ?local_forward
    ?dynamic_forward
    ?remote_forward
    ?user
    ?(no_shell = false)
    ?request_tty
    ?port
    ?(options = [])
    ?workdir
    ?env
    hostname
  =
  {
    hostname;
    config_file;
    stdin_null;
    fork_after_authentication;
    forward_x11;
    identity_file;
    local_forward;
    dynamic_forward;
    remote_forward;
    user;
    no_shell;
    request_tty;
    port;
    options;
    workdir;
    env;
  }

let maybe_get = function
  | None -> [| |]
  | Some(array) -> array

let maybe_map f = function
  | None -> None
  | Some(x) -> Some(f x)

let maybe_option flag = function
  | None -> None
  | Some(optarg) -> Some [| flag; optarg |]

let maybe_list f =
  maybe_map (fun lst -> Array.concat (List.map f lst))

let maybe_concat lst =
  Array.concat(List.map maybe_get lst)

let flag ontrue onfalse = function
  | true -> [| ontrue |]
  | false -> [| onfalse |]

let maybe_flag ontrue = function
  | true -> Some [| ontrue |]
  | false -> None

let ssh_argv {
    hostname;
    config_file;
    stdin_null;
    fork_after_authentication;
    forward_x11;
    identity_file;
    local_forward;
    dynamic_forward;
    remote_forward;
    user;
    no_shell;
    request_tty;
    port;
    options;
  } rashell_command =
  maybe_concat [
    Some [| Rashell_Configuration.ac_path_ssh; |];
    maybe_option "-F" config_file;
    maybe_flag "-n" stdin_null;
    maybe_flag "-f" fork_after_authentication;
    maybe_flag "-Y" forward_x11;
    maybe_option "-i" identity_file;
    maybe_option "-L" local_forward;
    maybe_option "-R" remote_forward;
    maybe_option "-D" dynamic_forward;
    maybe_option "-l" user;
    maybe_flag "-N" no_shell;
    begin match request_tty with
      | Some(true) -> Some[|"-t"|];
      | Some(false) -> Some[|"-T"|];
      | None -> None
    end;
    maybe_option "p" port;
    begin match options with
      | [] -> None
      | lst ->
          Some (Array.concat (List.map (fun opt -> [| "-o"; opt |]) lst));
    end;
    Some [|
      hostname;
      Rashell_Command.to_script rashell_command;
    |];
  ]

let command ({ workdir; env;} as options) rashell_command =
  Rashell_Command.command ?workdir ?env
    ("", ssh_argv options rashell_command)
