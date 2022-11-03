open Datetime

let encode_UTCDateOnly (x : fix_utcdateonly) : string =
  let y, m, d = T.to_date x in
  Printf.sprintf "%04d%02d%02d" (Z.to_int y) (Z.to_int m) (Z.to_int d)

let encode_LocalMktDate (x : fix_localmktdate) : string = encode_UTCDateOnly x

let encode_UTCTimeOnly_milli (x : fix_utctimeonly_milli) : string =
  let hh, mm, ss, ms = to_time_ms x in
  Printf.sprintf "%02d:%02d:%02d.%03d" (Z.to_int hh) (Z.to_int mm) (Z.to_int ss)
    (Z.to_int ms)

let encode_UTCTimeOnly_micro (x : fix_utctimeonly_micro) : string =
  let hh, mm, ss, us = to_time_us x in
  Printf.sprintf "%02d:%02d:%02d.%06d" (Z.to_int hh) (Z.to_int mm) (Z.to_int ss)
    (Z.to_int us)

let encode_UTCTimestamp_milli (x : fix_utctimestamp_milli) : string =
  let (y, m, d), (hh, mm, ss, ms) = to_date_time_ms x in
  Printf.sprintf "%04d%02d%02d-%02d:%02d:%02d.%03d" (Z.to_int y) (Z.to_int m)
    (Z.to_int d) (Z.to_int hh) (Z.to_int mm) (Z.to_int ss) (Z.to_int ms)

let encode_UTCTimestamp_micro (x : fix_utctimestamp_micro) : string =
  let (y, m, d), (hh, mm, ss, us) = to_date_time_us x in
  Printf.sprintf "%04d%02d%02d-%02d:%02d:%02d.%06d" (Z.to_int y) (Z.to_int m)
    (Z.to_int d) (Z.to_int hh) (Z.to_int mm) (Z.to_int ss) (Z.to_int us)

let encode_MonthYear ((t, w) : fix_monthyear) : string =
  let y, m, d = T.to_date t in
  let ym = Printf.sprintf "%04d%02d" (Z.to_int y) (Z.to_int m) in
  match w with
  | Some Week_1 -> ym ^ "w1"
  | Some Week_2 -> ym ^ "w2"
  | Some Week_3 -> ym ^ "w3"
  | Some Week_4 -> ym ^ "w4"
  | Some Week_5 -> ym ^ "w5"
  | None -> ym ^ Printf.sprintf "%02d" (Z.to_int d)

let encode_Duration (x : fix_duration) : string =
  let days = to_days x in
  "D" ^ Z.to_string days
