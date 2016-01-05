(* TestTimestamp -- Test Timestamps

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Printf
open Broken
open Rashell_Broken
open Rashell_Posix
open Lwt.Infix

type concrete_timestamp =
  | Text of string
  | Unix of Unix.tm
  | Epoch of float

module Concrete_Timestamp =
struct

  module Basis =
  struct
    type t = concrete_timestamp

    let pp_print fft =
      let open Format in
      function
      | Text(s) -> fprintf fft "Text(%S)" s
      | Unix(t) -> Unix.(fprintf fft "Unix(\"%04d-%02d-%02dT%02d:%02d:%02dZ\")"
                           (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec)
      | Epoch(s) -> fprintf fft "Epoch(%g)" s
  end

  include Basis
  include Mixture_Format.Make(Basis)

  let pp_sprint () x =
    to_string x

  let to_timestamp = function
  | Text(s) -> Rashell_Timestamp.of_string s
  | Unix(t) -> Rashell_Timestamp.of_unix t
  | Epoch(s) -> s
end


let assert_to_string (concrete, expected) =
  assert_equal
    (Format.sprintf "test_to_string (%a, %S)"
       Concrete_Timestamp.pp_sprint concrete expected)
    ~printer:Format.pp_print_string
    (fun x -> Rashell_Timestamp.to_string(Concrete_Timestamp.to_timestamp x))
    concrete expected

let assert_measure_interval (a, b, expected) =
  assert_equal
    (Format.sprintf "test_measure_interval (%a, %a, %f)"
       Concrete_Timestamp.pp_sprint a
       Concrete_Timestamp.pp_sprint b
       expected)
    ~printer:Format.pp_print_float
    (fun (s1, s2) -> Rashell_Timestamp.measure_interval
        (Concrete_Timestamp.to_timestamp s1)
        (Concrete_Timestamp.to_timestamp s2))
    (a, b) expected


let test_to_string () =
  let open Unix in
  List.map assert_to_string [
    Epoch(0.0), "1970-01-01T00:00:00.000Z";
    Unix({
        tm_sec = 0;
        tm_min = 0;
        tm_hour = 0;
        tm_mday = 1;
        tm_mon = 0;
        tm_year = 70;
        tm_wday = 4;
        tm_yday = 0;
        tm_isdst = false;
      }), "1970-01-01T00:00:00.000Z";
    Unix({
        tm_sec = 10;
        tm_min = 0;
        tm_hour = 0;
        tm_mday = 1;
        tm_mon = 0;
        tm_year = 83;
        tm_wday = 4;
        tm_yday = 0;
        tm_isdst = false;
      }), "1983-01-01T00:00:10.000Z";
    Text("1983-01-05T06:20:10.000Z"),
    "1983-01-05T06:20:10.000Z";
  ]

let test_measure_interval () =
  let open Unix in
  let seconds_per_hour = 3600.0 in
  let seconds_per_day = 24.0 *. seconds_per_hour in
  List.map assert_measure_interval [
    Unix({
        tm_sec = 10;
        tm_min = 0;
        tm_hour = 0;
        tm_mday = 1;
        tm_mon = 0;
        tm_year = 83;
        tm_wday = 4;
        tm_yday = 0;
        tm_isdst = false;
      }),
    Unix({
        tm_sec = 10;
        tm_min = 0;
        tm_hour = 0;
        tm_mday = 3;
        tm_mon = 0;
        tm_year = 83;
        tm_wday = 4;
        tm_yday = 0;
        tm_isdst = false;
      }), 2.0 *. seconds_per_day;
  ]



let () =
  make_suite "timestamp" "Test suite for timestamps"
  |@ test_to_string ()
  |@ test_measure_interval ()
  |> register
