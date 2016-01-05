(* Rashell_Posix -- General Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

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

module StatsCache =
struct
  module Cache = Hashtbl.Make(struct
      type t = string
      let equal a b =
        String.compare a b = 0
      let hash = Hashtbl.hash
    end)
  type t = stats Cache.t
  let create n =
    Cache.create n
  let _lookup stat acc f =
    try Cache.find acc f
    with Not_found ->
      let s = stat f in
      Cache.add acc f s;
      s
  let stat = _lookup Unix.stat
  let lstat = _lookup Unix.lstat
end


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

let rec _test_stat cache name x = function
  | Prune -> true
  | Has_kind(k) -> x.st_kind = k
  | Has_suffix(suff) -> string_match_glob ("*" ^ suff) suff
  | Is_owned_by_user(uid) -> x.st_uid = uid
  | Is_owned_by_group(gid) ->x.st_gid = gid
  | Is_newer_than(file) ->
      (StatsCache.stat cache file).st_mtime < x.st_mtime
  | Has_exact_permission(perm) -> x.st_perm = perm
  | Has_at_least_permission(perm) -> (x.st_perm land perm) = perm
  | Name(glob) -> string_match_glob glob name
  | And(lst) -> List.for_all (_test_stat cache name x) lst
  | Or(lst) -> List.exists (_test_stat cache name x) lst
  | Not(p) -> not(_test_stat cache name x p)

let rec _test_weight = function
  | Is_newer_than(_) -> 1
  | And(lst)
  | Or(lst) -> List.fold_left (fun n p -> n + _test_weight p) 0 lst
  | Not(p) -> _test_weight p
  | _ -> 0

let test ?(workdir = "") ?env ?(follow = false) predicate file =
  let actual_file =
    if Filename.is_relative file then
      Filename.concat workdir file
    else
      file
  in
  let cache = StatsCache.create (_test_weight predicate) in
  let stat () =
    (if follow then Lwt_unix.lstat else Lwt_unix.stat) actual_file
  in
  Lwt.try_bind stat
    (fun s -> Lwt.wrap (fun () -> _test_stat cache file s predicate))
    (fun _ -> Lwt.return_false)

let _destination path dest =
  Filename.concat dest (Filename.basename path)

let cp ?workdir ?env
    ?(follow = false) ?(force = false) ?(recursive = false) pathlst dest =
  let argv path = Array.concat [
      [| ac_path_cp; |];
      (flag "-H" follow);
      (flag "-f" force);
      (flag "-R" recursive);
      [| path; dest |];
    ]
  in
  let loop path =
    exec_utility (command ?workdir ?env (ac_path_cp, argv path))
    >|= fun _ -> _destination path dest
  in
  Lwt_stream.map_s loop (Lwt_stream.of_list pathlst)

let rm ?workdir ?env ?(force = false) ?(recursive = false) pathlst =
  let argv path = Array.concat [
      [| ac_path_rm; |];
      (flag "-f" force);
      (flag "-R" recursive);
      [| path |];
    ]
  in
  let loop path =
    exec_utility (command ?workdir ?env (ac_path_rm, argv path))
    >|= fun _ -> path
  in
  Lwt_stream.map_s loop (Lwt_stream.of_list pathlst)

let mv ?workdir ?env ?(force = false) pathlst dest =
  let argv path = Array.concat [
      [| ac_path_mv; |];
      (flag "-f" force);
      [| path; dest |]
    ]
  in
  let loop path =
    exec_utility (command ?workdir ?env (ac_path_mv, argv path))
    >|= fun _ -> _destination path dest
  in
  Lwt_stream.map_s loop (Lwt_stream.of_list pathlst)


let ln ?workdir ?env ?(force = false) ?(symbolic = false) pathlst dest =
  let argv path = Array.concat [
      [| ac_path_ln; "-v" |];
      (flag "-f" force);
      (flag "-s" symbolic);
      [| path; dest |]
    ]
  in
  let loop path =
    exec_utility (command ?workdir ?env (ac_path_ln, argv path))
    >|= fun _ -> _destination path dest
  in
  Lwt_stream.map_s loop (Lwt_stream.of_list pathlst)


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


type free_disk_space = {
  df_device: string;
  df_blocks: int;
  df_used: int;
  df_free: int;
  df_capacity: float;
  df_mounted_on: string;
}

let df_of_string s =
  let open Scanf in
  let cond_scanf scanners fallback channel =
    let rec loop = function
      | [] -> fallback channel
      | hd :: tl ->
        try hd channel
        with Scanf.Scan_failure(_) -> loop tl
    in
    loop scanners
  in
  let scan_device =
      cond_scanf
      [ fun sc -> bscanf sc "map %s" (fun s -> "map "^s) ]
      (fun sc -> bscanf sc "%s" (fun s -> s))
  in
  sscanf s "%r %d %d %d %f%% %s" scan_device
    (fun df_device df_blocks df_used df_free df_capacity df_mounted_on -> {
         df_device;
         df_blocks;
         df_used;
         df_free;
         df_capacity = df_capacity /. 100.0;
         df_mounted_on;
       })

let df paths =
  let s =
    exec_query (command ~env:[| "LANG=C" |]
                  ("", Array.append
                     [| ac_path_df; "-k"; "-P" |]
                     (Array.of_list paths)))
  in
  Lwt_stream.junk s
  >>= fun () ->
  Lwt_stream.to_list s
  >>= fun lst ->
  Lwt.return(List.map df_of_string lst)

let du paths =
  let of_string s =
    Scanf.sscanf s "%d %s" (fun n p -> (p, n))
  in
  exec_query (command ("", Array.append [| ac_path_du; "-s"; "-k" |]
                         (Array.of_list paths)))
  |> Lwt_stream.to_list
  >>= Lwt_list.map_s (Lwt.wrap1 of_string)
