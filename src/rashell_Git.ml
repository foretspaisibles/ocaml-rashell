(* Rashell_Git -- Git Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015—2016 Michael Grünewald

   This file must be used under the terms of the MIT license.
   This source file is licensed as described in the file LICENSE, which
   you should have received as part of this distribution. The terms
   are also available at
   https://opensource.org/licenses/MIT *)
open Lwt.Infix
open Rashell_Posix
open Rashell_Configuration
open Rashell_Command

module Maybe =
  Lemonade_Maybe

let ( / ) =
  Filename.concat

let flag f option = match option with
  | Some(true) -> [| f |]
  | _ -> [| |]

let option option moptarg = match moptarg with
  | Some(optarg) -> [| option; optarg |]
  | None -> [| |]

let option_no_space option moptarg = match moptarg with
  | Some(optarg) -> [| Printf.sprintf "%s=%s" option optarg |]
  | None -> [| |]

let maybe_transform f = function
  | None -> [| |]
  | Some(x) -> f(x)

let topleveldir ?workdir () =
  exec_utility ~chomp:true
    (command ?workdir ("", [| ac_path_git; "rev-parse"; "--show-toplevel" |]))

let _hook_dir_from_topleveldir dir =
  dir / ".git" / "hooks"

let _hook_dir ?workdir () =
  Lwt.map _hook_dir_from_topleveldir(topleveldir ?workdir ())

let hook_list ?workdir () =
  let predicate =
    And[
      Or[
        Has_kind S_REG;
        Has_kind S_LNK;
      ];
      Not(Name("*.*"))
    ]
  in
  _hook_dir ?workdir ()
  >>= fun dir ->
  Lwt_stream.to_list (find predicate [ dir ])

let hook_install ?workdir ?force name program =
  _hook_dir ?workdir ()
  >>= fun dir ->
  Lwt_stream.to_list (ln ?force ~symbolic:true [ program ] (dir / name))
  |> Lwt.map ignore

let hook_install_script ?workdir ?(force = false) ?(perm = 0o700) name script =
  _hook_dir ?workdir ()
  >>= fun dir ->
  Lwt_io.with_file
    ~flags:Unix.([O_WRONLY; O_CREAT; O_TRUNC] @(if force then [] else [ O_EXCL ]))
    ~perm
    ~mode:Lwt_io.output
    (dir / name) (fun ch -> Lwt_io.write ch script)

let hook_run ?workdir ?env ?(important = false) (cmd, argv) =
  topleveldir ?workdir ()
  >>= fun dir ->
  let hook = _hook_dir_from_topleveldir dir / cmd in
  test ~follow:true Prune hook
  >>= function
  | true -> exec_test (command ?workdir ?env (hook, argv))
  | false -> Lwt.return (not important)

let branch_checkout ?workdir ?env ?(create = false) ?start branch =
  let reallycreate = match create, start with
    | _, Some(_) -> Some(true)
    | true, None -> Some(true)
    | false, None -> Some(false)
  in
  let argv =
    Array.concat [
      [| ac_path_git; "checkout"; |];
      (flag "-B" reallycreate);
      [| branch |];
      (maybe_transform (fun x -> [| x |]) start)
    ]
  in
  exec_utility (command ?workdir ?env (ac_path_git, argv))
  >|= ignore

let branch_list ?workdir ?env ?merged ?all () =
  let open Str in
  let garbage = regexp "^[*] \\|^  " in
  let alias = regexp ".* -> " in
  let remove_garbage s =
    replace_first garbage "" s
  in
  let is_not_alias s =
    try ignore(search_forward alias s 0); false
    with Not_found -> true
  in
  let argv = Array.concat [
      [| ac_path_git; "branch"; "--list" |];
      (match merged with
       | Some(true) -> [| "--merged" |]
       | Some(false) -> [| "--no-merged" |]
       | None -> [| |]);
      (match all with
       | Some(true) -> [| "--all" |]
       | Some(false) -> [| "--remotes" |]
       | None -> [| |]);
    ]
  in
  Lwt_stream.to_list
    (exec_query (command ?workdir ?env (ac_path_git, argv)))
  >|= List.filter is_not_alias
  >|= List.map remove_garbage

let branch_current ?workdir ?env () =
  let argv = [| ac_path_git; "rev-parse"; "--abbrev-ref";  "HEAD" |] in
  exec_utility ~chomp:true (command ?workdir ?env (ac_path_git, argv))
  >>= function
  | "HEAD" -> Printf.ksprintf Lwt.fail_with "Rashell_Git.branch_current: %S: The repository is in detached HEAD state."
                (match workdir with Some(path) -> path | None -> ".")
  | branch -> Lwt.return branch

let is_in_detached_head_state ?workdir ?env () =
  let argv = [| ac_path_git; "rev-parse"; "--abbrev-ref";  "HEAD" |] in
  exec_utility ~chomp:true (command ?workdir ?env (ac_path_git, argv))
  >|= function
  | "HEAD" -> true
  | _ -> false


let tag_create ?workdir ?env ?gpg_sign ?message tag =
  let argv =
    Array.concat [
      [| ac_path_git; "commit"; |];
      (flag "--sign" gpg_sign);
      (option "-m" message);
      [| tag |];
    ]
  in
  exec_utility (command ?workdir ?env (ac_path_git, argv))
  >|= ignore


let tag_list ?workdir ?env () =
  let argv = [| ac_path_git; "tag"; "--list" |]; in
  Lwt_stream.to_list
    (exec_query (command ?workdir ?env (ac_path_git, argv)))


let clone ?workdir ?env ?template ?bare ?mirror ?origin ?branch ?(config = []) ?depth ?single_branch ?recursive ?reference ?dissociate ?destdir repository =
  let open Printf in
  let configuration =
    Array.concat(List.map (fun (k,v) -> [| "--config"; sprintf "%s=%s" k v; |]) config)
  in
  let argv =
    Array.concat [
      [| ac_path_git; "clone"; |];
      (option "--template" template);
      (flag "--bare" bare);
      (flag "--mirror" mirror);
      (option "--origin" origin);
      (option "--branch" branch);
      configuration;
      (option "--depth" (Maybe.map string_of_int depth));
      (flag "--single-branch" single_branch);
      (flag "--recursive" recursive);
      (option "--reference" reference);
      (flag "--dissociate" dissociate);
      [| repository |];
      (match destdir with
       | Some(x) -> [| x |]
       | None -> [| |]);
    ]
  in
  exec_utility
    (command ?workdir (ac_path_git,  argv))
  >>= fun _ -> Lwt.return_unit

let add ?workdir ?env paths =
  let argv = Array.concat [
      [| ac_path_git; "add"; |];
      Array.of_list paths;
    ]
  in
  exec_utility (command ?workdir ?env (ac_path_git, argv))
  >|= ignore

let commit ?workdir ?env ?gpg_sign message =
  let argv =
    Array.concat [
      [| ac_path_git; "commit"; |];
      (flag "--gpg-sign" gpg_sign);
      [| "-m"; message |]
    ]
  in
  exec_utility (command ?workdir ?env (ac_path_git, argv))
  >|= ignore

let merge_no_fast_forward ?workdir ?env
    ?no_commit ?squash ?strategy ?strategy_option ?verify_signatures ?gpg_sign
    ?message commits =
  let argv =
    Array.concat [
      [| ac_path_git; "merge"; "--no-ff" |];
      (flag "--no-commit" no_commit);
      (flag "--squash" squash);
      (option_no_space "--strategy" strategy);
      (option_no_space "--strategy-option" strategy_option);
      (flag "--verify-signatures" verify_signatures);
      (flag "--gpg-sign" gpg_sign);
      (option "-m" message);
      Array.of_list commits;
    ]
  in
  exec_utility (command ?workdir ?env (ac_path_git, argv))
  >|= ignore
