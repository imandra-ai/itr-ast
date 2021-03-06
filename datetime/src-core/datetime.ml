open Ints
open Imandra_prelude
module T = Imandra_ptime
module TE = Imandra_ptime_extra

(** Helpers *)

let of_date_time_ms (date, (hh, mm, ss, ms)) =
  let time = hh, mm, ss in
  let tz = zero in
  T.of_date_time (date, (time, tz))
  |> Option.flat_map (fun t -> T.add_span t (TE.Span.of_int_ms ms))

let of_time_ms time_ms =
  let date = epoch_year, one, one in
  of_date_time_ms (date, time_ms)

let to_date_time_ms (t : T.t) =
  let (y, m, d), ((hh, mm, ss), _tz) = T.to_date_time t in
  let _d, ps = t |> T.to_span |> T.Span.to_d_ps in
  let ps = ps mod ps_count_in_s in
  let ms = ps / ps_count_in_ms in
  (y, m, d), (hh, mm, ss, ms)

let to_time_ms (t : T.t) =
  let _date, time_ms = to_date_time_ms t in
  time_ms

let of_date_time_us (date, (hh, mm, ss, us)) =
  let time = hh, mm, ss in
  let tz = zero in
  T.of_date_time (date, (time, tz))
  |> Option.flat_map (fun t -> T.add_span t (TE.Span.of_int_us us))

let of_time_us time_us =
  let date = epoch_year, one, one in
  of_date_time_us (date, time_us)

let to_date_time_us (t : T.t) =
  let (y, m, d), ((hh, mm, ss), _tz) = T.to_date_time t in
  let _d, ps = t |> T.to_span |> T.Span.to_d_ps in
  let ps = ps mod ps_count_in_s in
  let us = ps / ps_count_in_us in
  (y, m, d), (hh, mm, ss, us)

let to_time_us (t : T.t) =
  let _date, time_us = to_date_time_us t in
  time_us

let to_millis (t : T.t) = T.truncate ~frac_s:three t

let to_micros (t : T.t) = T.truncate ~frac_s:six t

let to_days (d : T.Span.t) = T.Span.to_int_s d / s_count_in_day

type fix_utctimestamp_milli = T.t
(** UTC Timestamp type. *)

type fix_utctimestamp_micro = T.t

(** Default timestamp is 0 unix epoch timestamp *)
let default_utctimestamp_milli : fix_utctimestamp_milli = T.epoch

(** Default timestamp is 0 unix epoch timestamp *)
let default_utctimestamp_micro : fix_utctimestamp_micro = T.epoch

(** Constructor for the UTC timestamp milli  *)
let make_utctimestamp_milli (year : int) (month : int) (day : int) (hour : int)
    (minute : int) (second : int) (millisec : int option) :
    fix_utctimestamp_milli option =
  let ms = millisec |> Option.get_or ~default:zero in
  of_date_time_ms ((year, month, day), (hour, minute, second, ms))

let make_utctimestamp_milli_unsafe (year : int) (month : int) (day : int)
    (hour : int) (minute : int) (second : int) (millisec : int option) :
    fix_utctimestamp_milli =
  make_utctimestamp_milli year month day hour minute second millisec
  |> Option.get_or ~default:default_utctimestamp_milli

(** Constructor for the UTC timestamp micro  *)
let make_utctimestamp_micro (year : int) (month : int) (day : int) (hour : int)
    (minute : int) (second : int) (microsec : int option) :
    fix_utctimestamp_micro option =
  let us = microsec |> Option.get_or ~default:zero in
  of_date_time_us ((year, month, day), (hour, minute, second, us))

let make_utctimestamp_micro_unsafe (year : int) (month : int) (day : int)
    (hour : int) (minute : int) (second : int) (microsec : int option) :
    fix_utctimestamp_micro =
  make_utctimestamp_micro year month day hour minute second microsec
  |> Option.get_or ~default:default_utctimestamp_micro

(** Conversion for the UTC timestamp and timeonly fields *)
let convert_utctimestamp_milli_micro (f1 : fix_utctimestamp_milli) :
    fix_utctimestamp_micro =
  f1

let convert_utctimestamp_micro_milli (f1 : fix_utctimestamp_micro) :
    fix_utctimestamp_milli =
  to_millis f1

(** Checking validity of the values in the UTC timestamp milli *)
let is_valid_utctimestamp_milli (ts : fix_utctimestamp_milli) =
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  TE.is_valid ts && ps mod ps_count_in_ms = zero

(** Checking validity of the values in the UTC timestamp micro *)
let is_valid_utctimestamp_micro (ts : fix_utctimestamp_micro) =
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  TE.is_valid ts && ps mod ps_count_in_us = zero

(** Two UTC timestamps are equal if they have equal field values *)
let utctimestamp_Equal_milli_milli (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_milli) =
  T.equal t1 t2

let utctimestamp_Equal_micro_micro (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_micro) =
  T.equal t1 t2

let utctimestamp_Equal_milli_micro (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_micro) =
  T.equal t1 (to_millis t2)

let utctimestamp_Equal_micro_milli (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_milli) =
  T.equal (to_millis t2) t1

(** UTC Timestamp milli base comparison operator *)
let utctimestamp_GreaterThan_milli_milli (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_milli) =
  T.compare t1 t2 > zero

(** UTC Timestamp milli base comparison operator *)
let utctimestamp_GreaterThan_micro_micro (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 t2 > zero

(** UTC Timestamp base comparison operator - milli to micro *)
let utctimestamp_GreaterThan_milli_micro (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 (to_millis t2) > zero

(** UTC Timestamp base comparison operator - micro to milli *)
let utctimestamp_GreaterThan_micro_milli (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_milli) =
  T.compare (to_millis t1) t2 > zero

let utctimestamp_GreaterThanEqual_milli_milli (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_milli) =
  T.compare t1 t2 >= zero

let utctimestamp_GreaterThanEqual_micro_micro (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 t2 >= zero

let utctimestamp_GreaterThanEqual_milli_micro (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 (to_millis t2) >= zero

let utctimestamp_GreaterThanEqual_micro_milli (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_milli) =
  T.compare (to_millis t1) t2 >= zero

let utctimestamp_LessThan_milli_milli (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_milli) =
  T.compare t1 t2 < zero

let utctimestamp_LessThan_micro_micro (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 t2 < zero

let utctimestamp_LessThan_milli_micro (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 (to_millis t2) < zero

let utctimestamp_LessThan_micro_milli (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_milli) =
  T.compare (to_millis t1) t2 < zero

let utctimestamp_LessThanEqual_milli_milli (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_milli) =
  T.compare t1 t2 <= zero

let utctimestamp_LessThanEqual_micro_micro (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 t2 <= zero

let utctimestamp_LessThanEqual_milli_micro (t1 : fix_utctimestamp_milli)
    (t2 : fix_utctimestamp_micro) =
  T.compare t1 (to_millis t2) <= zero

let utctimestamp_LessThanEqual_micro_milli (t1 : fix_utctimestamp_micro)
    (t2 : fix_utctimestamp_milli) =
  T.compare (to_millis t1) t2 <= zero

type fix_localmktdate = T.t
(** LocalMktDate type denotes a particular date*)

(** Default LocalMktDate corresponds to 0 unix epoch timestamp *)
let default_localmktdate : fix_localmktdate = T.epoch

(** LocalMktDate constructor *)
let make_localmktdate (year : int) (month : int) (day : int) :
    fix_localmktdate option =
  T.of_date (year, month, day)

let make_localmktdate_unsafe (year : int) (month : int) (day : int) :
    fix_localmktdate =
  make_localmktdate year month day
  |> Option.get_or ~default:default_localmktdate

let is_valid_localmktdate (lmd : fix_localmktdate) =
  let _d, ps = T.Span.to_d_ps (T.to_span lmd) in
  TE.is_valid lmd && ps = zero

let localmktdate_GreaterThan (lmd1 : fix_localmktdate) (lmd2 : fix_localmktdate)
    =
  T.compare lmd1 lmd2 > zero

let localmktdate_LessThan (lmd1 : fix_localmktdate) (lmd2 : fix_localmktdate) =
  T.compare lmd1 lmd2 < zero

let localmktdate_Equal (lmd1 : fix_localmktdate) (lmd2 : fix_localmktdate) =
  T.compare lmd1 lmd2 = zero

let localmktdate_GreaterThanEqual (lmd1 : fix_localmktdate)
    (lmd2 : fix_localmktdate) =
  T.compare lmd1 lmd2 >= zero

let localmktdate_LessThanEqual (lmd1 : fix_localmktdate)
    (lmd2 : fix_localmktdate) =
  T.compare lmd1 lmd2 <= zero

(** Week *)
type fix_week =
  | Week_1
  | Week_2
  | Week_3
  | Week_4
  | Week_5

let compare_week w1 w2 =
  if w1 = w2 then
    zero
  else (
    match w1, w2 with
    | Week_1, _ -> neg_one
    | Week_2, Week_1 -> one
    | Week_2, _ -> neg_one
    | Week_3, (Week_1 | Week_2) -> one
    | Week_3, _ -> neg_one
    | Week_4, (Week_1 | Week_2 | Week_3) -> one
    | Week_4, _ -> neg_one
    | Week_5, _ -> one
  )

type fix_monthyear = T.t * fix_week option
(** MonthYear

    YYYYMM
    YYYYMMDD
    YYYYMMw1

    if fix_week is Some, we ignore the day.

    TODO: represent YYYYMM (no week or day)
 *)

let default_monthyear = T.epoch, None

let make_monthyear (year : int) (month : int) (week : fix_week option) :
    fix_monthyear option =
  match T.of_date (year, month, one) with
  | Some t -> Some (t, week)
  | None -> None

let make_monthyear_unsafe year month week : fix_monthyear =
  make_monthyear year month week |> Option.get_or ~default:default_monthyear

let is_valid_monthyear ((t, week) : fix_monthyear) =
  let _d, ps = T.to_span t |> T.Span.to_d_ps in
  let _y, _m, d = T.to_date t in
  TE.is_valid t && ps = zero
  &&
  match week with
  | Some _ -> d = one
  | None -> true

let compare_monthyear ((t1, w1) : fix_monthyear) ((t2, w2) : fix_monthyear) =
  let c = T.compare t1 t2 in
  if c <> zero then
    c
  else (
    match w1, w2 with
    | Some w1, Some w2 -> compare_week w1 w2
    | Some _, None -> one
    | None, Some _ -> neg_one
    | None, None -> zero
  )

let monthyear_GreaterThan (my1 : fix_monthyear) (my2 : fix_monthyear) =
  compare_monthyear my1 my2 > zero

let monthyear_LessThan (my1 : fix_monthyear) (my2 : fix_monthyear) =
  compare_monthyear my1 my2 < zero

let monthyear_Equal (my1 : fix_monthyear) (my2 : fix_monthyear) =
  compare_monthyear my1 my2 = zero

let monthyear_GreaterThanEqual (my1 : fix_monthyear) (my2 : fix_monthyear) =
  compare_monthyear my1 my2 >= zero

let monthyear_LessThanEqual (my1 : fix_monthyear) (my2 : fix_monthyear) =
  compare_monthyear my1 my2 <= zero

type fix_utctimeonly_milli = T.t
(** UTC Timeonly *)

type fix_utctimeonly_micro = T.t

let default_utctimeonly_milli = T.epoch

let default_utctimeonly_micro = T.epoch

let make_utctimeonly_milli (hour : int) (minute : int) (second : int)
    (millisec : int option) : fix_utctimeonly_milli option =
  let ms = millisec |> Option.get_or ~default:zero in
  of_time_ms (hour, minute, second, ms)

let make_utctimeonly_milli_unsafe hour minute second millisec =
  make_utctimeonly_milli hour minute second millisec
  |> Option.get_or ~default:default_utctimeonly_milli

let make_utctimeonly_micro (hour : int) (minute : int) (second : int)
    (microsec : int option) : fix_utctimeonly_micro option =
  let us = microsec |> Option.get_or ~default:zero in
  of_time_us (hour, minute, second, us)

let make_utctimeonly_micro_unsafe hour minute second microsec =
  make_utctimeonly_micro hour minute second microsec
  |> Option.get_or ~default:default_utctimeonly_micro

let convert_utctimeonly_milli_micro (f1 : fix_utctimeonly_milli) :
    fix_utctimeonly_micro =
  f1

let convert_utctimeonly_micro_milli (f1 : fix_utctimeonly_micro) :
    fix_utctimeonly_milli =
  to_millis f1

let is_valid_utctimeonly_milli (t : fix_utctimeonly_milli) =
  let d, ps = T.Span.to_d_ps (T.to_span t) in
  TE.is_valid t && d = zero && ps mod ps_count_in_ms = zero

let is_valid_utctimeonly_micro (t : fix_utctimeonly_micro) =
  let d, ps = T.Span.to_d_ps (T.to_span t) in
  TE.is_valid t && d = zero && ps mod ps_count_in_us = zero

let utctimeonly_Equal_milli_milli (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_milli) =
  T.equal t1 t2

let utctimeonly_Equal_micro_micro (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_micro) =
  T.equal t1 t2

let utctimeonly_Equal_milli_micro (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_micro) =
  T.equal t1 (to_millis t2)

let utctimeonly_Equal_micro_milli (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_milli) =
  T.equal (to_millis t1) t2

let utctimeonly_GreaterThan_milli_milli (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_milli) =
  T.compare t1 t2 > zero

let utctimeonly_GreaterThan_micro_micro (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 t2 > zero

let utctimeonly_GreaterThan_milli_micro (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 (to_millis t2) > zero

let utctimeonly_GreaterThan_micro_milli (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_milli) =
  T.compare (to_millis t1) t2 > zero

let utctimeonly_LessThan_milli_milli (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_milli) =
  T.compare t1 t2 < zero

let utctimeonly_LessThan_micro_micro (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 t2 < zero

let utctimeonly_LessThan_milli_micro (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 (to_millis t2) < zero

let utctimeonly_LessThan_micro_milli (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_milli) =
  T.compare (to_millis t1) t2 < zero

let utctimeonly_LessThanEqual_milli_milli (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_milli) =
  T.compare t1 t2 <= zero

let utctimeonly_LessThanEqual_micro_micro (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 t2 <= zero

let utctimeonly_LessThanEqual_milli_micro (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 (to_millis t2) <= zero

let utctimeonly_LessThanEqual_micro_milli (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_milli) =
  T.compare (to_millis t1) t2 <= zero

let utctimeonly_GreaterThanEqual_milli_milli (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_milli) =
  T.compare t1 t2 >= zero

let utctimeonly_GreaterThanEqual_micro_micro (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 t2 >= zero

let utctimeonly_GreaterThanEqual_milli_micro (t1 : fix_utctimeonly_milli)
    (t2 : fix_utctimeonly_micro) =
  T.compare t1 (to_millis t2) >= zero

let utctimeonly_GreaterThanEqual_micro_milli (t1 : fix_utctimeonly_micro)
    (t2 : fix_utctimeonly_milli) =
  T.compare (to_millis t1) t2 >= zero

type fix_utcdateonly = T.t
(** UTC Dateonly *)

(** UTC Dateonly *)
let default_utcdateonly = T.epoch

let make_utcdateonly (year : int) (month : int) (day : int) :
    fix_utcdateonly option =
  T.of_date (year, month, day)

let make_utcdateonly_unsafe y m d =
  make_utcdateonly y m d |> Option.get_or ~default:default_utcdateonly

let is_valid_utcdateonly (d : fix_utcdateonly) =
  let _d, ps = d |> T.to_span |> T.Span.to_d_ps in
  ps = zero && TE.is_valid d

let utcdateonly_GreaterThan (d1 : fix_utcdateonly) (d2 : fix_utcdateonly) =
  T.compare d1 d2 > zero

let utcdateonly_LessThan (d1 : fix_utcdateonly) (d2 : fix_utcdateonly) =
  T.compare d1 d2 < zero

let utcdateonly_Equal (d1 : fix_utcdateonly) (d2 : fix_utcdateonly) =
  T.equal d1 d2

let utcdateonly_GreaterThanEqual (d1 : fix_utcdateonly) (d2 : fix_utcdateonly) =
  T.compare d1 d2 >= zero

let utcdateonly_LessThanEqual (d1 : fix_utcdateonly) (d2 : fix_utcdateonly) =
  T.compare d1 d2 <= zero

type fix_duration = T.Span.t
(** Duration
    TODO: calendar durations rather than POSIX time spans?
    E.g. add 1 month to Jan 31 => ??
         add 1 year to Feb 29 => ??
 *)

let is_valid_duration (dur : fix_duration) = TE.Span.is_valid dur

(** Months are assumed to be 30 days *)
let make_duration years months days hours minutes seconds : fix_duration =
  let d = (years * Z.of_int 365) + (months * Z.of_int 30) + days in
  let secs = (hours * Z.of_int 3600) + (minutes * Z.of_int 60) + seconds in
  T.Span.(add (unsafe_of_d_ps (d, Z.of_int 0)) (of_int_s secs))

(** Overflows/underflows are silently ignored *)
let utctimestamp_milli_duration_Add (t : fix_utctimestamp_milli)
    (dur : fix_duration) : fix_utctimestamp_milli =
  T.add_span t dur |> Option.get_or ~default:t

(** Overflows/underflows are silently ignored *)
let duration_utctimestamp_milli_Add (dur : fix_duration)
    (t : fix_utctimestamp_milli) : fix_utctimestamp_milli =
  T.add_span t dur |> Option.get_or ~default:t

let utctimestamp_micro_duration_Add (t : fix_utctimestamp_micro)
    (dur : fix_duration) : fix_utctimestamp_micro =
  T.add_span t dur |> Option.get_or ~default:t

let duration_utctimestamp_micro_Add (dur : fix_duration)
    (t : fix_utctimestamp_micro) : fix_utctimestamp_micro =
  T.add_span t dur |> Option.get_or ~default:t

let seconds_to_duration seconds : fix_duration = T.Span.of_int_s seconds

let duration_to_seconds dur : int = T.Span.to_int_s dur

let duration_Equal d1 d2 = T.Span.equal d1 d2

let duration_GreaterThan d1 d2 = T.Span.compare d1 d2 > zero

let duration_GreaterThanEqual d1 d2 = T.Span.compare d1 d2 >= zero

let duration_LessThan d1 d2 = T.Span.compare d1 d2 < zero

let duration_LessThanEqual d1 d2 = T.Span.compare d1 d2 <= zero

let convert_utctimestamp_milli_utctimeonly_milli (ts : fix_utctimestamp_milli) :
    fix_utctimeonly_milli =
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (zero, ps)

let convert_utctimestamp_micro_utctimeonly_micro (ts : fix_utctimestamp_micro) :
    fix_utctimeonly_micro =
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (zero, ps)

let convert_utctimestamp_milli_utctimeonly_micro (ts : fix_utctimestamp_milli) :
    fix_utctimeonly_micro =
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (zero, ps)

let convert_utctimestamp_micro_utctimeonly_milli (ts : fix_utctimestamp_micro) :
    fix_utctimeonly_milli =
  let ts = to_millis ts in
  let _d, ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (zero, ps)

let convert_utctimestamp_micro_localmktdate (ts : fix_utctimestamp_micro) :
    fix_localmktdate =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, zero)

let convert_utctimestamp_milli_localmktdate (ts : fix_utctimestamp_milli) :
    fix_localmktdate =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, zero)

let convert_utctimestamp_micro_utcdateonly (ts : fix_utctimestamp_micro) :
    fix_utcdateonly =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, zero)

let convert_utctimestamp_milli_utcdateonly (ts : fix_utctimestamp_milli) :
    fix_utcdateonly =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, zero)

let convert_utctimestamp_micro_monthyear (ts : fix_utctimestamp_micro) :
    fix_monthyear =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  let t = T.unsafe_of_d_ps (d, zero) in
  t, None

let convert_utctimestamp_milli_monthyear (ts : fix_utctimestamp_milli) :
    fix_monthyear =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  let t = T.unsafe_of_d_ps (d, zero) in
  t, None

let make_utctimestamp_micro_utctimeonly_micro_utcdateonly
    (to_ : fix_utctimeonly_micro) (do_ : fix_utcdateonly) :
    fix_utctimestamp_micro =
  let d, _ps = do_ |> T.to_span |> T.Span.to_d_ps in
  let _d, ps = to_ |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, ps)

let make_utctimestamp_milli_utctimeonly_milli_utcdateonly
    (to_ : fix_utctimeonly_milli) (do_ : fix_utcdateonly) :
    fix_utctimestamp_milli =
  let d, _ps = do_ |> T.to_span |> T.Span.to_d_ps in
  let _d, ps = to_ |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, ps)

let make_utctimestamp_micro_utctimeonly_milli_utcdateonly
    (to_ : fix_utctimeonly_milli) (do_ : fix_utcdateonly) :
    fix_utctimestamp_micro =
  let d, _ps = do_ |> T.to_span |> T.Span.to_d_ps in
  let _d, ps = to_ |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, ps)

let make_utctimestamp_milli_utctimeonly_micro_utcdateonly
    (to_ : fix_utctimeonly_micro) (do_ : fix_utcdateonly) :
    fix_utctimestamp_milli =
  let d, _ps = do_ |> T.to_span |> T.Span.to_d_ps in
  let to_ = to_millis to_ in
  let _d, ps = to_ |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, ps)
