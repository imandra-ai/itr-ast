(* LEGACY DECODERS FOR BACK-COMPATIBILITY *)
module I = Itr_ast
open Decoders_yojson.Basic.Decode

let int_decoder : Z.t Decoders_yojson.Basic.Decode.decoder =
  field "Type" string >>= fun x ->
  match x with
  | "int" -> field "Value" string >>= fun v -> succeed (Z.of_string v)
  | _ -> fail "expected int type tag."

let epoch_year = Z.of_int 1970

let of_date_time_ps (date, (hh, mm, ss, ps)) =
  let time = hh, mm, ss in
  let tz = Z.zero in
  Imandra_ptime.of_date_time (date, (time, tz))
  |> CCOption.flat_map (fun t ->
         Imandra_ptime.add_span t (Imandra_ptime_extra.Span.of_int_ps ps))

let of_time_ps time_ps =
  let date = epoch_year, Z.one, Z.one in
  of_date_time_ps (date, time_ps)

(** Constructor for the UTC timestamp pico  *)
let make_utctimestamp_pico (year : Z.t) (month : Z.t) (day : Z.t) (hour : Z.t)
    (minute : Z.t) (second : Z.t) (picosec : Z.t option) :
    Imandra_ptime.t option =
  let ps = picosec |> CCOption.get_or ~default:Z.zero in
  of_date_time_ps ((year, month, day), (hour, minute, second, ps))

let make_utctimeonly_pico (hour : Z.t) (minute : Z.t) (second : Z.t)
    (picosec : Z.t option) : Imandra_ptime.t option =
  let ps = picosec |> CCOption.get_or ~default:Z.zero in
  of_time_ps (hour, minute, second, ps)

(* older version used microsecs *)
let utctimestamp_micro_decoder :
    Imandra_ptime.t Decoders_yojson.Basic.Decode.decoder =
  field "utc_timestamp_year" int_decoder >>= fun utc_timestamp_year ->
  field "utc_timestamp_month" int_decoder >>= fun utc_timestamp_month ->
  field "utc_timestamp_day" int_decoder >>= fun utc_timestamp_day ->
  field "utc_timestamp_hour" int_decoder >>= fun utc_timestamp_hour ->
  field "utc_timestamp_minute" int_decoder >>= fun utc_timestamp_minute ->
  field "utc_timestamp_second" int_decoder >>= fun utc_timestamp_second ->
  maybe (field "utc_timestamp_microsec" int_decoder)
  >>= fun utc_timestamp_microsec ->
  succeed
    (make_utctimestamp_pico utc_timestamp_year utc_timestamp_month
       utc_timestamp_day utc_timestamp_hour utc_timestamp_minute
       utc_timestamp_second
       (CCOption.map (fun x -> Z.(x * of_int 1000000)) utc_timestamp_microsec)
    |> CCOption.get_or ~default:Imandra_ptime.epoch)

let utctimeonly_micro_decoder :
    Imandra_ptime.t Decoders_yojson.Basic.Decode.decoder =
  field "utc_timeonly_hour" int_decoder >>= fun utc_timeonly_hour ->
  field "utc_timeonly_minute" int_decoder >>= fun utc_timeonly_minute ->
  field "utc_timeonly_second" int_decoder >>= fun utc_timeonly_second ->
  maybe (field "utc_timeonly_microsec" int_decoder)
  >>= fun utc_timeonly_microsec ->
  succeed
    (make_utctimeonly_pico utc_timeonly_hour utc_timeonly_minute
       utc_timeonly_second
       (CCOption.map (fun x -> Z.(x * of_int 1000000)) utc_timeonly_microsec)
    |> CCOption.get_or ~default:Imandra_ptime.epoch)

(* *)
