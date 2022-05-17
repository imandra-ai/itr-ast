open Ints
open Datetime

let zfill (n : int) (s : string) : string =
  let bv = Bytes.of_string s in
  let length = Bytes.length bv in
  if n <= length then Bytes.to_string bv
  else
    let result = Bytes.make n '0' in
    Bytes.blit bv 0 result (n - length) length;
    Bytes.to_string result

let encode_UTCDateOnly (x : fix_utcdateonly) : string =
  let y, m, d = T.to_date x in
  Printf.sprintf "%s%s%s"
    (zfill 4 @@ Z.to_string y)
    (zfill 2 @@ Z.to_string m)
    (zfill 2 @@ Z.to_string d)

let encode_LocalMktDate (x : fix_localmktdate) : string = encode_UTCDateOnly x

let encode_UTCTimeOnly_milli (x : fix_utctimeonly_milli) : string =
  let hh, mm, ss, ms = to_time_ms x in
  let hms =
    Printf.sprintf "%s:%s:%s"
      (zfill 2 @@ Z.to_string hh)
      (zfill 2 @@ Z.to_string mm)
      (zfill 2 @@ Z.to_string ss)
  in
  if ms = zero then hms
  else hms ^ Printf.sprintf ".%s" (zfill 3 @@ Z.to_string ms)

let encode_UTCTimeOnly_micro (x : fix_utctimeonly_micro) : string =
  let hh, mm, ss, us = to_time_us x in
  let hms =
    Printf.sprintf "%s:%s:%s"
      (zfill 2 @@ Z.to_string hh)
      (zfill 2 @@ Z.to_string mm)
      (zfill 2 @@ Z.to_string ss)
  in
  if us = zero then hms
  else hms ^ Printf.sprintf ".%s" (zfill 6 @@ Z.to_string us)

let encode_UTCTimestamp_milli (x : fix_utctimestamp_milli) : string =
  let (y, m, d), (hh, mm, ss, ms) = to_date_time_ms x in
  let ymdhms =
    Printf.sprintf "%s%s%s-%s:%s:%s"
      (zfill 4 @@ Z.to_string y)
      (zfill 2 @@ Z.to_string m)
      (zfill 2 @@ Z.to_string d)
      (zfill 2 @@ Z.to_string hh)
      (zfill 2 @@ Z.to_string mm)
      (zfill 2 @@ Z.to_string ss)
  in
  if ms = zero then ymdhms
  else ymdhms ^ Printf.sprintf ".%s" (zfill 3 @@ Z.to_string ms)

let encode_UTCTimestamp_micro (x : fix_utctimestamp_micro) : string =
  let (y, m, d), (hh, mm, ss, us) = to_date_time_us x in
  let ymdhms =
    Printf.sprintf "%s%s%s-%s:%s:%s"
      (zfill 4 @@ Z.to_string y)
      (zfill 2 @@ Z.to_string m)
      (zfill 2 @@ Z.to_string d)
      (zfill 2 @@ Z.to_string hh)
      (zfill 2 @@ Z.to_string mm)
      (zfill 2 @@ Z.to_string ss)
  in
  if us = zero then ymdhms
  else ymdhms ^ Printf.sprintf ".%s" (zfill 6 @@ Z.to_string us)

let encode_MonthYear ((t, w) : fix_monthyear) : string =
  let y, m, d = T.to_date t in
  let ym =
    Printf.sprintf "%s%s" (zfill 4 @@ Z.to_string y) (zfill 2 @@ Z.to_string m)
  in
  match w with
  | Some Week_1 -> ym ^ "w1"
  | Some Week_2 -> ym ^ "w2"
  | Some Week_3 -> ym ^ "w3"
  | Some Week_4 -> ym ^ "w4"
  | Some Week_5 -> ym ^ "w5"
  | None -> ym ^ Printf.sprintf "%s" (zfill 2 @@ Z.to_string d)

let encode_Duration (x : fix_duration) : string =
  let days = to_days x in
  "D" ^ Z.to_string days
