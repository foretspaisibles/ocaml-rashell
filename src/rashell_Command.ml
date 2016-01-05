(* Rashell_Command -- Computations consuming a subprocess

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Lwt.Infix

let lwt_bind2 m1 m2 f =
  Lwt.bind m1 (fun x1 -> Lwt.bind m2 (f x1))

let maybe_return x =
  Lwt.return(Some(x))

let maybe_fail _ =
  Lwt.return_none

let send_lines p data =
  let channel = p#stdin in
  Lwt.finalize
    (fun () -> Lwt_io.write_lines channel data)
    (fun () -> Lwt_io.close channel)

let recv_anything f p channel =
  let streamf _ =
    Lwt.catch
      (function () -> f channel >>= maybe_return)
      (function
        | Unix.Unix_error (Unix.EPIPE, _, _)
        | End_of_file ->
            Lwt_io.close channel >>= fun _ -> Lwt.return_none
        | exn -> Lwt.fail exn)
  in
  Gc.finalise (fun x -> ignore(Lwt_io.close x)) channel;
  Lwt_stream.from streamf

let recv_chars p channel =
  recv_anything Lwt_io.read_char p channel

let recv_lines p channel =
  recv_anything Lwt_io.read_line p channel

type 'a monitor_state =
  | Monitor_Initial
  | Monitor_Remember of 'a option Lwt.t
  | Monitor_Final

let monitor sender stream =
  let sentinel = sender >>= maybe_fail in
  let state = ref Monitor_Initial in
  let streamf () =
    match !state with
    | Monitor_Initial ->
        let getter =
          Lwt.apply Lwt_stream.get stream
          >>= fun got ->
          match got with
          | Some(x) -> Lwt.return_some x
          | None -> sentinel
        in
        let answer _ =
          match Lwt.state sentinel with
             | Lwt.Sleep -> getter
             | Lwt.Return _ -> (state := Monitor_Final; getter)
             | Lwt.Fail _ -> (state := Monitor_Remember(getter); sentinel)
           in
           Lwt.try_bind (fun () -> Lwt.choose [sentinel; getter]) answer answer
    | Monitor_Remember(getter) -> (state := Monitor_Final; getter)
    | Monitor_Final -> Lwt_stream.get stream
  in
  Lwt_stream.from streamf

let string_chomp s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then
    String.sub s 0 (n - 1)
  else
    s

let string_split delim s =
  let q = Queue.create () in
  let b = Buffer.create 100 in
  let add () =
    (Queue.add (Buffer.contents b) q; Buffer.clear b)
  in
  let loop c =
    if c = delim then
      add ()
    else
      Buffer.add_char b c
  in
  String.iter loop s;
  add ();
  List.rev(Queue.fold (fun ax item -> item :: ax) [] q)

let is_debugged command =
  List.mem command
    (try string_split ':' (Sys.getenv "RASHELL_DEBUG")
     with Not_found -> [])


type process_status = Unix.process_status  =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type t = {
  program: string;
  argv: string array;
  env: string array option;
  workdir: string option;
}

let command_name cmd =
  try
    List.hd(List.rev(string_split '/'
                       (if cmd.program = "" then
                          cmd.argv.(0) else cmd.program)))
  with _ -> ""


exception Error of t * process_status * string


let pp_print_process_status fft = function
  | WEXITED(n) -> Format.fprintf fft "WEXITED(%d)" n
  | WSIGNALED(n) -> Format.fprintf fft "WSIGNALED(%d)" n
  | WSTOPPED(n) -> Format.fprintf fft "WSTOPPED(%d)" n


let pp_print_command fft cmd =
  let pp_print_field fft key value =
    Format.fprintf fft "@[<2>%s = %S;@]@ " key value
  in
  let pp_print_array fft key a =
    Format.fprintf fft "@[<2>%s = [@[<2>%a@]];@]@ " key
      (fun fft a -> Array.iter (fun s -> Format.fprintf fft "@ %S;" s) a)
      a
  in
  let pp_print_fields fft cmd =
    pp_print_field fft "program" cmd.program;
    pp_print_array fft "argv" cmd.argv;
    (match cmd.env with
     | None -> ()
     | Some(env) -> pp_print_array fft "env" env);
    (match cmd.workdir with
     | None -> ()
     | Some(workdir) -> pp_print_field fft "workdir" workdir);
  in
  Format.fprintf fft "@[<2>{ %a}@]" pp_print_fields cmd

let pp_print_error fft = function
  | Error(command, process_status, stderr) ->
      Some(Format.fprintf fft "%s.Error(%a, %a, %S)"
             "Rashell_Command"
             pp_print_command command
             pp_print_process_status process_status
             stderr)
  | _ -> None

let maybe_map f = function
  | Some(x) -> Some(f x)
  | None -> None

let () =
  Printexc.register_printer
    (fun exn -> maybe_map
        Format.flush_str_formatter
        (pp_print_error Format.str_formatter exn))

let command ?workdir ?env (program, argv) = {
  program;
  argv;
  env;
  workdir;
}

let outcome is_test cmd p =
  lwt_bind2
    (Lwt_stream.to_string (recv_chars p p#stderr))
    p#status
    (fun stderr status ->
       if is_debugged (command_name cmd) then
         Format.eprintf "@[<2>%s: outcome:@ %a@ %a@ %s@]@\n%!"
           "Rashell_Command"
           pp_print_command cmd
           pp_print_process_status status
           stderr;
       match status, is_test with
       | WEXITED(0), _ -> Lwt.return_unit
       | WEXITED(1), true -> Lwt.return_unit
       | _ -> Lwt.fail(Error(cmd, status, stderr)))

let with_workdir path f x =
  let currentdir = Sys.getcwd () in
  let () = Sys.chdir path in
  let answer = f x in
  let () = Sys.chdir currentdir in
  answer

let open_process cmd =
  let open_process_in_cwd cmd =
    Lwt_process.open_process_full ?env:cmd.env (cmd.program, cmd.argv)
  in
  if is_debugged (command_name cmd) then
    Format.eprintf "@[<2>%s: open_process:@ %a@]@\n%!"
      "Rashell_Command" pp_print_command cmd;
  match cmd.workdir with
  | None -> open_process_in_cwd cmd
  | Some(otherdir) -> with_workdir otherdir open_process_in_cwd cmd

let supervise f cmd =
  try f cmd
  with Sys_error(mesg) -> Lwt.fail (Error(cmd, WEXITED(127), mesg))

let exec_utility_unsafe ?(chomp = false) cmd =
  let p = open_process cmd in
  Lwt_stream.to_string (recv_chars p p#stdout)
  >>= fun a ->
  (outcome false cmd p
   >>= fun () ->
   if chomp then
     Lwt.return(string_chomp a)
   else
     Lwt.return a)

let exec_utility ?chomp cmd =
  supervise (exec_utility_unsafe ?chomp) cmd

let exec_test_unsafe cmd =
  let p = open_process cmd in
  Lwt_stream.to_string (recv_chars p p#stdout)
  >>= fun _ ->
  p#status
  >>= fun status ->
  (outcome true cmd p >>= fun () ->
   match status with
   | WEXITED(0) -> Lwt.return_true
   | WEXITED(1) -> Lwt.return_false
   | _ -> Printf.ksprintf Lwt.fail_with "%s.exec_test_unsafe" "Rashell_Command")

let exec_test cmd =
  supervise exec_test_unsafe cmd

let exec_query cmd =

  try let p = open_process cmd in
    monitor (outcome false cmd p) (recv_lines p p#stdout)
  with Sys_error(mesg) ->
    Lwt_stream.from (fun () -> Lwt.fail(Error(cmd, WEXITED(127), mesg)))

let exec_filter cmd lines =
  try let p = open_process cmd in
    let sender =
      let p1 = send_lines p lines
      and p2 = outcome false cmd p
      in
      lwt_bind2 p1 p2 (fun () () -> Lwt.return_unit)
    in
    monitor sender (recv_lines p p#stdout)
  with Sys_error(mesg) ->
    Lwt_stream.from (fun () -> Lwt.fail(Error(cmd, WEXITED(127), mesg)))

let exec_shell_unsafe cmd =
  let p =
    let open_process cmd =
      Lwt_process.open_process_none ?env:cmd.env (cmd.program, cmd.argv)
    in
    match cmd.workdir with
    | None -> open_process cmd
    | Some(otherdir) -> with_workdir otherdir open_process cmd
  in
  p#status
  >>= fun status ->
  match status with
  | WEXITED(0) -> Lwt.return_unit
  | _ -> Printf.ksprintf Lwt.fail_with "%s.exec_shell_unsafe" "Rashell_Command"

let exec_shell cmd =
  supervise exec_shell_unsafe cmd

let which ?path file =
  let actual_path = match path with
    | Some(path) -> path
    | None -> (try string_split ':' (Sys.getenv "PATH") with Not_found -> [])
  in
  let loop ax path =
    match ax with
    | Some(_) -> ax
    | None -> ( if Sys.file_exists(Filename.concat path file) then
                  Some(Filename.concat path file)
                else
                  None)
  in
  Lwt.return(List.fold_left loop None actual_path)

let expand_path name =
  let open Str in
  let buf = Buffer.create (String.length name) in
  let getenv key =
    try Sys.getenv key
    with Not_found -> ""
  in
  let homedir_other user =
    try Unix.((getpwnam user).pw_dir)
    with Not_found ->
      Printf.ksprintf failwith "Rashell_Command.expand_path: %S: Cannot find user in the passwd database." user
  in
  let homedir_self () =
    let passwd () =
      try Some(Unix.((getpwuid (getuid())).pw_dir))
      with Not_found -> None
    in
    let home () =
      try Some(Sys.getenv "HOME")
      with Not_found -> None
    in
    let homedrive () =
      try Some(Filename.concat (Sys.getenv "HOMEDRIVE") (Sys.getenv "HOMEPATH"))
      with Not_found -> None
    in
    let loop ax f =
      match ax with
      | None -> f ()
      | Some(_) -> ax
    in
    match List.fold_left loop None [ passwd; home; homedrive ] with
    | Some(homedir) -> homedir
    | None -> "/"
  in
  let newname =
    substitute_first
      (regexp "^~[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-]*")
      (fun s ->
         let tildexpr = matched_string s in
         if String.length tildexpr > 1 then
           homedir_other (string_after tildexpr 1)
         else
           homedir_self())
      name
  in
  Buffer.add_substitute buf getenv newname;
  Buffer.contents buf

let chomp s =
  string_chomp s

(* Finite automatons recognising globbing patterns. *)
module Glob =
struct

  let rec list_match pattern text =
    match pattern, text with
    | [], [] -> true
    | '*' :: pattern_tl, [] -> list_match pattern_tl []
    | '*' :: pattern_tl, text_hd :: text_tl ->
        list_match pattern_tl text || list_match pattern text_tl
    | '?' :: pattern_tl, _ :: text_tl -> list_match pattern_tl text_tl
    | pattern_hd :: pattern_tl, text_hd :: text_tl ->
        (pattern_hd = text_hd) && list_match pattern_tl text_tl
    | _ -> false

  let string_chars s =
    let rec loop ax i =
      if i < 0 then
        ax
      else
        loop (s.[i] :: ax) (i-1)
    in
    loop [] (String.length s - 1)

  let string_match pattern text =
    list_match (string_chars pattern) (string_chars text)
end

let string_match_glob pattern text =
  Glob.string_match pattern text
