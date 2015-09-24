(* Rashell_Timestamp -- Timestamp operation

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
