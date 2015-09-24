(* Rashell_Timestamp -- Timestamp operation

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Unix
open Printf
open Scanf

type t = float

let offset =
  let t0 = gettimeofday () in
  let t1 = fst(mktime(gmtime t0)) in
  t0 -. t1

let now () =
  gettimeofday()

let to_unix timestamp =
  gmtime timestamp

let of_unix x =
  fst(mktime(x)) +. offset

let of_string s =
  let convert year month day hour minute seconds =
    let unixtm = {
      tm_sec = 0;
      tm_min = minute;
      tm_hour = hour;
      tm_mday = day;
      tm_mon = month;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    } in
    (of_unix unixtm) +. seconds
  in
  try sscanf s "%d-%d-%dT%d:%d:%fZ" convert
  with Scan_failure(mesg) ->
    ksprintf failwith "%s.of_string: %s" __MODULE__ mesg

let to_string timestamp =
  let open Unix in
  let tm = gmtime timestamp in
  let sec =
    timestamp -. of_unix { (localtime timestamp) with tm_sec = 0 }
  in
  sprintf "%04d-%02d-%02dT%02d:%02d:%06.3fZ"
    (1900 + tm.tm_year)
    (1 + tm.tm_mon)
    tm.tm_mday tm.tm_hour tm.tm_min sec

let measure_interval timestart timestop =
  let a = timestart in
  let b =  timestop in
  b -. a

let compare =
  Pervasives.compare
