(* Rashell_Command -- Computations consuming a subprocess

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


(* The definitions packed in the module Lwt_process_internals are
   derived from the definitions with the same name found in the
   module Lwt_process, © 2009 by Jérémie Dimino, http://www.ocsigen.org/lwt. *)

module Lwt_process_internals =
struct
  let ignore_close ch =
    ignore (Lwt_io.close ch)

  let send f p data =
    let channel = p#stdin in
    Lwt.finalize
      (fun () -> f channel data)
      (fun () -> Lwt_io.close channel)

  let read_opt read ic =
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> read ic >|= fun x -> Some x)
      (function
        | Unix.Unix_error (Unix.EPIPE, _, _) | End_of_file ->
            Lwt.return_none
        | exn -> Lwt.fail exn)

  let recv_chars pr ic =
    let open Lwt.Infix in
    Gc.finalise ignore_close ic;
    Lwt_stream.from
      (fun _ ->
         read_opt Lwt_io.read_char ic >>= fun x ->
         if x = None then begin
           Lwt_io.close ic >>= fun () ->
           Lwt.return x
         end else
           Lwt.return x)

  let recv_lines pr ic =
    let open Lwt.Infix in
    Gc.finalise ignore_close ic;
    Lwt_stream.from
      (fun _ ->
         read_opt Lwt_io.read_line ic >>= fun x ->
         if x = None then begin
           Lwt_io.close ic >>= fun () ->
           Lwt.return x
         end else
           Lwt.return x)

  type 'a map_state =
    | Init
    | Save of 'a option Lwt.t
    | Done


  (* Monitor the thread [sender] in the stream [st] so write errors are
     reported. When the stream is exhausted, we wait for the sender
     to terminate.

     In this variant of the original function, we wait for join on the
     sender at the end of the stream, so that we have the chance to
     see any error it would throw. *)

  let monitor sender st =
    let open Lwt.Infix in
    let sender = sender >|= fun () -> None in
    let state = ref Init in
    Lwt_stream.from
      (fun () ->
         match !state with
         | Init ->
             let getter =
               Lwt.apply Lwt_stream.get st
               >>= function
               | None -> (sender >>= fun _ -> Lwt.return_none)
               | Some(x) -> Lwt.return_some x
             in
             let result _ =
               match Lwt.state sender with
               | Lwt.Sleep ->
                   (* The sender is still sleeping, behave as the
                      getter. *)
                   getter
               | Lwt.Return _ ->
                   (* The sender terminated successfully, we are
                      done monitoring it. *)
                   state := Done;
                   getter
               | Lwt.Fail _ ->
                   (* The sender failed, behave as the sender for
                      this element and save current getter. *)
                   state := Save getter;
                   sender
             in
             Lwt.try_bind (fun () -> Lwt.choose [sender; getter]) result result
         | Save t ->
             state := Done;
             t
         | Done ->
             Lwt_stream.get st)
end

open Lwt.Infix
open Lwt_process_internals

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
             __MODULE__
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
  let%lwt stderr = Lwt_stream.to_string (recv_chars p p#stderr) in
  let%lwt status = p#status in
  if is_debugged (command_name cmd) then
    Format.eprintf "@[<2>%s: outcome:@ %a@ %a@ %s@]@\n%!"
      __MODULE__
      pp_print_command cmd
      pp_print_process_status status
      stderr;
  match status, is_test with
  | WEXITED(0), _ -> Lwt.return_unit
  | WEXITED(1), true -> Lwt.return_unit
  | _ -> Lwt.fail(Error(cmd, status, stderr))

let open_process cmd =
  let with_workdir path f x =
      let currentdir = Sys.getcwd () in
      let () = Sys.chdir path in
      let answer = f x in
      let () = Sys.chdir currentdir in
      answer
  in
  let open_process_in_cwd cmd =
    Lwt_process.open_process_full ?env:cmd.env (cmd.program, cmd.argv)
  in
  if is_debugged (command_name cmd) then
    Format.eprintf "@[<2>%s: open_process:@ %a@]@\n%!"
      __MODULE__ pp_print_command cmd;
  match cmd.workdir with
  | None -> open_process_in_cwd cmd
  | Some(otherdir) -> with_workdir otherdir open_process_in_cwd cmd

let supervise f cmd =
  try f cmd
  with Sys_error(mesg) -> Lwt.fail (Error(cmd, WEXITED(127), mesg))

let exec_utility_unsafe cmd =
  let p = open_process cmd in
  let%lwt a = Lwt_stream.to_string (recv_chars p p#stdout) in
  let%lwt () = outcome false cmd p in
  Lwt.return a

let exec_utility cmd =
  supervise exec_utility_unsafe cmd

let exec_test_unsafe cmd =
  let p = open_process cmd in
  let%lwt _ = Lwt_stream.to_string (recv_chars p p#stdout) in
  let%lwt status = p#status in
  let%lwt () = outcome true cmd p in
  match status with
  | WEXITED(0) -> Lwt.return_true
  | WEXITED(1) -> Lwt.return_false
  | _ -> Printf.ksprintf Lwt.fail_with "%s.exec_test_unsafe" __MODULE__

let exec_test cmd =
  supervise exec_test_unsafe cmd

let exec_query cmd =
  match open_process cmd with
  | p ->  monitor (outcome false cmd p) (recv_lines p p#stdout)
  | exception Sys_error(mesg) ->
      Lwt_stream.from (fun () -> Lwt.fail(Error(cmd, WEXITED(127), mesg)))

let exec_filter cmd lines =
  match open_process cmd with
  | p ->
      let sender =
        let%lwt () = send Lwt_io.write_lines p lines
        and () = outcome false cmd p in
        Lwt.return_unit
      in
      monitor sender (recv_lines p p#stdout)

  | exception Sys_error(mesg) ->
      Lwt_stream.from (fun () -> Lwt.fail(Error(cmd, WEXITED(127), mesg)))

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
  let home =
    try Sys.getenv "HOME"
    with Not_found -> "/"
  in
  let getenv key =
    try Sys.getenv key
    with Not_found -> ""
  in
  let newname =
    substitute_first
      (regexp "^~[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-]*")
      (fun s -> if String.length s > 0 then
          (replace_first (regexp "//*[.][.]//*") "/../"
             (home ^ "/../" ^ (string_after s 1)))
        else
          home)
      name
  in
  Buffer.add_substitute buf getenv newname;
  Buffer.contents buf
