open Datetime

let parse_date str =
  if String.length str <> 8 then
    None
  else (
    try
      Scanf.sscanf str "%04u%02u%02u" (fun y m d ->
          Some (Z.of_int y, Z.of_int m, Z.of_int d))
    with _ -> None
  )

let parse_time_ms str =
  let length = String.length str in
  if length <> 8 && length <> 12 then
    None
  else (
    try
      Scanf.sscanf str "%02u:%02u:%02u%s" (fun h m s ms ->
          let ms =
            if ms = "" then
              None
            else
              Scanf.sscanf ms ".%03d" (fun x -> Some (Z.of_int x))
          in
          Some (Z.of_int h, Z.of_int m, Z.of_int s, ms))
    with _ -> None
  )

let parse_time_us str =
  let length = String.length str in
  if length <> 8 && length <> 15 then
    None
  else (
    try
      Scanf.sscanf str "%02u:%02u:%02u%s" (fun h m s us ->
          let us =
            if us = "" then
              None
            else
              Scanf.sscanf us ".%06d" (fun x -> Some (Z.of_int x))
          in
          Some (Z.of_int h, Z.of_int m, Z.of_int s, us))
    with _ -> None
  )

let parse_UTCDateOnly str : fix_utcdateonly option =
  parse_date str |> CCOption.flat_map T.of_date

let parse_LocalMktDate str : fix_localmktdate option =
  parse_date str |> CCOption.flat_map T.of_date

let parse_UTCTimeOnly_milli str : fix_utctimeonly_milli option =
  parse_time_ms str
  |> CCOption.flat_map (fun (hh, mm, ss, ms) ->
         make_utctimeonly_milli hh mm ss ms)

let parse_UTCTimeOnly_micro str : fix_utctimeonly_micro option =
  parse_time_us str
  |> CCOption.flat_map (fun (hh, mm, ss, us) ->
         make_utctimeonly_micro hh mm ss us)

let parse_UTCTimestamp_milli str : fix_utctimestamp_milli option =
  let length = String.length str in
  if length <> 17 && length <> 21 then
    None
  else (
    try
      let date, time =
        Scanf.sscanf str "%s@-%s" (fun d t -> parse_date d, parse_time_ms t)
      in
      match date, time with
      | Some (y, m, d), Some (hh, mm, ss, ms) ->
        make_utctimestamp_milli y m d hh mm ss ms
      | _ -> None
    with _ -> None
  )

let parse_UTCTimestamp_micro str =
  let length = String.length str in
  if length <> 17 && length <> 24 then
    None
  else (
    try
      let date, time =
        Scanf.sscanf str "%s@-%s" (fun d t -> parse_date d, parse_time_us t)
      in
      match date, time with
      | Some (y, m, d), Some (hh, mm, ss, us) ->
        make_utctimestamp_micro y m d hh mm ss us
      | _ -> None
    with _ -> None
  )

let parse_MonthYear str =
  let length = String.length str in
  if length < 6 then
    None
  else (
    try
      let y, m, tail =
        Scanf.sscanf str "%04u%02u%s" (fun y m t -> Z.of_int y, Z.of_int m, t)
      in
      let _d, week =
        match tail with
        | "w1" -> Z.one, Some Week_1
        | "w2" -> Z.one, Some Week_2
        | "w3" -> Z.one, Some Week_3
        | "w4" -> Z.one, Some Week_4
        | "w5" -> Z.one, Some Week_5
        | "" -> Z.one, None
        | day -> Z.of_string day, None
      in
      make_monthyear y m week
    with _ -> None
  )

let parse_Duration (str : string) : fix_duration option =
  let parse_int (str : string) : Z.t option =
    try Some (Z.of_string str) with _ -> None
  in
  if String.length str > 1 then (
    try
      let t, v = Scanf.sscanf str "%c%s" (fun t v -> t, v) in
      match t, v with
      | 'D', x ->
        (match parse_int x with
        | None -> None
        | Some x -> Some (make_duration Z.zero Z.zero x Z.zero Z.zero Z.zero))
      | 'M', x ->
        (match parse_int x with
        | None -> None
        | Some x -> Some (make_duration Z.zero x Z.zero Z.zero Z.zero Z.zero))
      | 'W', x ->
        (match parse_int x with
        | None -> None
        | Some x ->
          Some
            (make_duration Z.zero Z.zero Z.(x * of_int 7) Z.zero Z.zero Z.zero))
      | 'Y', x ->
        (match parse_int x with
        | None -> None
        | Some x -> Some (make_duration x Z.zero Z.zero Z.zero Z.zero Z.zero))
      | _, _ -> None
    with _ -> None
  ) else
    None