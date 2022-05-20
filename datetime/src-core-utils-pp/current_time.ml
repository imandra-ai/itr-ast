open Datetime

let get_current_utctimestamp_micro () : fix_utctimestamp_micro =
  Ptime_clock.now () |> T.of_ptime |> to_micros
