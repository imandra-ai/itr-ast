open Yojson.Basic
open Datetime
module JU = Yojson.Basic.Util
module D = Decoders_yojson.Basic.Decode
open D.Infix

let filter_nulls x =
  List.filter
    (function
      | _, `Null -> false
      | _ -> true)
    x

let span_to_json (s : T.span) =
  let d, ps = T.Span.to_d_ps s in
  `List [ `String (Z.to_string d); `String (Z.to_string ps) ]

let span_decoder : T.span D.decoder =
  D.list D.string >>= function
  | [ d; ps ] ->
    (match T.Span.of_d_ps (Z.of_string d, Z.of_string ps) with
    | Some s -> D.succeed s
    | None -> D.fail "invalid time span")
  | _ -> D.fail "expected [d, ps]"

let ptime_to_json (t : T.t) = span_to_json (T.to_span t)

let ptime_decoder : T.t D.decoder =
  span_decoder >>= fun s ->
  match T.of_span s with
  | Some t -> D.succeed t
  | None -> D.fail "invalid timestamp"

let validate_with f msg decoder =
  decoder >>= fun x ->
  if f x then
    D.succeed x
  else
    D.fail msg

let utctimestamp_milli_to_json (ts : fix_utctimestamp_milli) = ptime_to_json ts

let utctimestamp_milli_decoder :
    fix_utctimestamp_milli Decoders_yojson.Basic.Decode.decoder =
  ptime_decoder
  |> validate_with is_valid_utctimestamp_milli "fix_utctimestamp_milli"

let utctimestamp_micro_to_json (ts : fix_utctimestamp_micro) = ptime_to_json ts

let utctimestamp_micro_decoder :
    fix_utctimestamp_micro Decoders_yojson.Basic.Decode.decoder =
  ptime_decoder
  |> validate_with is_valid_utctimestamp_micro "fix_utctimestamp_micro"

let utctimestamp_milli_opt_to_json = function
  | None -> `Null
  | Some x -> utctimestamp_milli_to_json x

let utctimestamp_micro_opt_to_json = function
  | None -> `Null
  | Some x -> utctimestamp_micro_to_json x

let duration_to_json (d : fix_duration) = span_to_json d

let duration_decoder : fix_duration Decoders_yojson.Basic.Decode.decoder =
  span_decoder

let duration_opt_to_json = function
  | None -> `Null
  | Some x -> duration_to_json x

let localmktdate_to_json (d : fix_localmktdate) = ptime_to_json d

let localmktdate_decoder : fix_localmktdate Decoders_yojson.Basic.Decode.decoder
    =
  ptime_decoder |> validate_with is_valid_localmktdate "fix_localmktdate"

let localmktdate_opt_to_json = function
  | None -> `Null
  | Some x -> localmktdate_to_json x

let utcdateonly_to_json (d : fix_utcdateonly) = ptime_to_json d

let utcdateonly_decoder : fix_utcdateonly Decoders_yojson.Basic.Decode.decoder =
  ptime_decoder |> validate_with is_valid_utcdateonly "fix_utcdateonly"

let utcdateonly_opt_to_json = function
  | None -> `Null
  | Some x -> utcdateonly_to_json x

(** UTC Timeonly *)
let utctimeonly_milli_to_json (d : fix_utctimeonly_milli) = ptime_to_json d

let utctimeonly_milli_decoder :
    fix_utctimeonly_milli Decoders_yojson.Basic.Decode.decoder =
  ptime_decoder
  |> validate_with is_valid_utctimeonly_milli "fix_utctimeonly_milli"

let utctimeonly_milli_opt_to_json = function
  | None -> `Null
  | Some x -> utctimeonly_milli_to_json x

let utctimeonly_micro_to_json (d : fix_utctimeonly_micro) = ptime_to_json d

let utctimeonly_micro_decoder :
    fix_utctimeonly_micro Decoders_yojson.Basic.Decode.decoder =
  ptime_decoder
  |> validate_with is_valid_utctimeonly_micro "fix_utctimeonly_micro"

let utctimeonly_micro_opt_to_json = function
  | None -> `Null
  | Some x -> utctimeonly_micro_to_json x

let week_to_json = function
  | Week_1 -> `String "Week1"
  | Week_2 -> `String "Week2"
  | Week_3 -> `String "Week3"
  | Week_4 -> `String "Week4"
  | Week_5 -> `String "Week5"

let week_decoder : fix_week Decoders_yojson.Basic.Decode.decoder =
  D.string >>= fun w ->
  match w with
  | "Week1" -> D.succeed Week_1
  | "Week2" -> D.succeed Week_2
  | "Week3" -> D.succeed Week_3
  | "Week4" -> D.succeed Week_4
  | "Week5" -> D.succeed Week_5
  | x -> D.fail (x ^ " is not a valid Week encoding.")

let week_opt_to_json = function
  | None -> `Null
  | Some x -> week_to_json x

let monthyear_to_json ((t, w) : fix_monthyear) =
  let list_assoc =
    [ "t", ptime_to_json t; "week", week_opt_to_json w ] |> filter_nulls
  in
  `Assoc list_assoc

let monthyear_decoder : fix_monthyear Decoders_yojson.Basic.Decode.decoder =
  D.field "t" ptime_decoder >>= fun t ->
  D.maybe (D.field "week" week_decoder) >>= fun w ->
  let my = t, w in
  if is_valid_monthyear my then
    D.succeed my
  else
    D.fail "invalid fix_monthyear"

let monthyear_opt_to_json = function
  | None -> `Null
  | Some x -> monthyear_to_json x
