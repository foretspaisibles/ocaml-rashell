(* Rashell_Posix -- General Commands

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
open Rashell_Configuration
open Rashell_Command

type file_kind = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

type file_perm = Unix.file_perm

type stats = Unix.stats = {
  st_dev: int;
  st_ino: int;
  st_kind: file_kind;
  st_perm: file_perm;
  st_nlink: int;
  st_uid: int;
  st_gid: int;
  st_rdev: int;
  st_size: int;
  st_atime: float;
  st_mtime: float;
  st_ctime: float;
}

type predicate =
  | Prune
  | Has_kind of file_kind
  | Has_suffix of string
  | Is_owned_by_user of int
  | Is_owned_by_group of int
  | Is_newer_than of string
  | Has_exact_permission of int
  | Has_at_least_permission of int
  | Name of string
  | And of predicate list
  | Or of predicate list
  | Not of predicate

let progname () =
  Filename.basename Sys.executable_name

let flag f option = match option with
  | true -> [| f |]
  | false -> [| |]

let maybe_transform f = function
  | None -> [| |]
  | Some(x) -> f(x)

let rec find_predicate_to_argv = function
  | Prune -> [| "-prune" |]
  | Has_kind(k) -> [| "-type"; find_letter_of_file_kind k |]
  | Has_suffix(suff) -> [| "-name"; "*" ^ suff |]
  | Is_owned_by_user(uid) -> [| "-uid"; string_of_int uid |]
  | Is_owned_by_group(gid) -> [| "-gid"; string_of_int gid |]
  | Is_newer_than(file) -> [| "-newer"; file |]
  | Has_exact_permission(perm) -> [| "-perm"; Printf.sprintf "%o" perm |]
  | Has_at_least_permission(perm) -> [| "-perm"; Printf.sprintf "-%o" perm |]
  | Name(glob) -> [| "-name"; glob |]
  | And(lst) -> combine "-a" lst
  | Or(lst) -> combine "-o" lst
  | Not(p) -> Array.concat [ [| "!" |]; (find_predicate_to_argv p) ]
and find_letter_of_file_kind = function
  | S_REG -> "f"
  | S_DIR -> "d"
  | S_CHR -> "c"
  | S_BLK -> "b"
  | S_LNK -> "l"
  | S_FIFO -> "p"
  | S_SOCK -> "s"
and combine operator lst =
  let n = List.length lst in
  let init i =
    match i, i mod 2 = 0, i / 2 with
    | 0, _, _ -> [| "(" |]
    | _, true, _ -> (if i = 2*n then [| ")" |] else [| operator |])
    | _, false, k -> find_predicate_to_argv (List.nth lst k)
  in
  Array.concat (Array.to_list (Array.init (2*n + 1) init))

let find ?workdir ?env
    ?(follow = false) ?(depthfirst = false) ?(onefilesystem = false)
    p pathlst =
  let argv = Array.concat [
      [| ac_path_find |];
      (flag "-L" follow);
      (flag "-d" depthfirst);
      (flag "-x" onefilesystem);
      (match pathlst with [] -> [| "." |] | _ -> Array.of_list pathlst);
      (find_predicate_to_argv p)
    ]
  in
  exec_query (command ?workdir ?env (ac_path_find, argv))

let cp ?workdir ?env
    ?(follow = false) ?(force = false) ?(recursive = false) pathlst dest =
  let argv = Array.concat [
      [| ac_path_cp; "-v" |];
      (flag "-H" follow);
      (flag "-f" force);
      (flag "-R" recursive);
      Array.of_list pathlst;
      [| dest |];
    ]
  in
  match pathlst with
  | [] -> Lwt_stream.of_list []
  | _ -> exec_query (command ?workdir ?env (ac_path_cp, argv))

let rm ?workdir ?env ?(force = false) ?(recursive = false) pathlst =
  let argv = Array.concat [
      [| ac_path_rm; "-v" |];
      (flag "-f" force);
      (flag "-R" recursive);
      Array.of_list pathlst;
    ]
  in
  match pathlst with
  | [] -> Lwt_stream.of_list []
  | _ -> exec_query(command ?workdir ?env (ac_path_rm, argv))

let mv ?workdir ?env ?(force = false) pathlst dest =
  let argv = Array.concat [
      [| ac_path_mv; "-v" |];
      (flag "-f" force);
      Array.of_list pathlst;
      [| dest |]
    ]
  in
  match pathlst with
  | [] -> Lwt_stream.of_list []
  | _ -> exec_query(command ?workdir ?env (ac_path_mv, argv))


let ln ?workdir ?env ?(force = false) ?(symbolic = false) pathlst dest =
  let argv = Array.concat [
      [| ac_path_ln; "-v" |];
      (flag "-f" force);
      (flag "-s" symbolic);
      Array.of_list pathlst;
      [| dest |]
    ]
  in
  match pathlst with
  | [] -> Lwt_stream.of_list []
  | _ -> exec_query(command ?workdir ?env (ac_path_ln, argv))


let sed ?workdir ?env ?(echo = true) script pathlst =
  let argv = Array.concat [
      [| ac_path_sed |];
      (flag "-n" (not echo));
      [| "-e"; script |];
      (Array.of_list pathlst);
    ]
  in
  match pathlst with
  | [] -> Lwt_stream.of_list []
  | _ -> exec_query(command ?workdir ?env (ac_path_sed, argv))

let sed_inplace ?workdir ?env ?(suffix = "") script pathlst =
  let argv = Array.concat [
      [| ac_path_sed |];
      [| "-i" ^ suffix |];
      [| "-e"; script |];
      (Array.of_list pathlst);
    ]
  in
  match pathlst with
  | [] -> Lwt.return_unit
  | _ -> (exec_utility (command ?workdir ?env (ac_path_sed, argv))
          >>= fun _ -> Lwt.return_unit)

let sed_filter ?workdir ?env ?(echo = true) script =
  let argv = Array.concat [
      [| ac_path_sed |];
      (flag "-n" (not echo));
      [| "-e"; script |];
    ]
  in
  exec_filter (command ?workdir ?env (ac_path_sed, argv))

let _awk_argv ?fs ?(bindings = []) script pathlst =
  Array.concat ([
      [| ac_path_awk; |];
      (maybe_transform (fun x -> [| "-F"; x |]) fs);
    ]
      @ (List.map (fun (k,v) -> [| "-v"; k ^"="^ v |]) bindings)
      @ [ [| script |] ]
      @ [ Array.of_list pathlst ])

let awk ?workdir ?env ?fs ?bindings script pathlst =
  exec_query
    (command ?workdir ?env
       (ac_path_awk, _awk_argv ?fs ?bindings script pathlst))

let awk_filter ?workdir ?env ?fs ?bindings script =
  exec_filter
    (command ?workdir ?env
       (ac_path_awk, _awk_argv ?fs ?bindings script []))
