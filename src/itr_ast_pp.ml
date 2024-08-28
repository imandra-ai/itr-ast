open Format
open Itr_ast
module T = Imandra_ptime
module TE = Imandra_ptime_extra

let ps_count_in_s = Z.of_int 1_000_000_000_000

let pp_bullet_list pp1 pp2 =
  CCFormat.(
    vbox
      (list
         (fun fmt (x, y) ->
           fprintf fmt "%a: - %a" (hvbox ~i:2 pp1) x (hvbox ~i:2 pp2) y)
         ~sep:(return "@,")))

let s_count_in_day = 86_400

let to_date_time_ps (t : T.t) =
  let (y, m, d), ((hh, mm, ss), _tz) = T.to_date_time t in
  let _d, ps = t |> T.to_span |> T.Span.to_d_ps in
  let ps = Z.(ps mod ps_count_in_s) in
  (y, m, d), (hh, mm, ss, ps)

let timestamp_pp ppf (x : T.t) =
  let (y, m, d), (hh, mm, ss, ps) = to_date_time_ps x in
  fprintf ppf "%04d%02d%02d-%02d:%02d:%02d.%12d" (Z.to_int y) (Z.to_int m)
    (Z.to_int d) (Z.to_int hh) (Z.to_int mm) (Z.to_int ss) (Z.to_int ps)

let to_time_ps (t : T.t) =
  let _date, time_ps = to_date_time_ps t in
  time_ps

let timeonly_pp ppf (x : T.t) =
  let hh, mm, ss, ps = to_time_ps x in
  fprintf ppf "%02d:%02d:%02d.%09d" (Z.to_int hh) (Z.to_int mm) (Z.to_int ss)
    (Z.to_int ps)

let dateonly_pp ppf (x : T.t) =
  let y, m, d = T.to_date x in
  fprintf ppf "%04d%02d%02d" (Z.to_int y) (Z.to_int m) (Z.to_int d)

let week_to_string = function
  | TE.Week_1 -> "Week1"
  | TE.Week_2 -> "Week2"
  | TE.Week_3 -> "Week3"
  | TE.Week_4 -> "Week4"
  | TE.Week_5 -> "Week5"

let monthyear_pp ppf ((t, w) : T.t * TE.week option) =
  let y, m, d = T.to_date t in
  fprintf ppf "%04d%02d%s" (Z.to_int y) (Z.to_int m)
    (match w with
    | Some w -> week_to_string w
    | None -> Printf.sprintf "%02d" (Z.to_int d))

let to_days_seconds (d : T.Span.t) =
  ( Z.to_int (T.Span.to_int_s d) / s_count_in_day,
    Z.to_int (T.Span.to_int_s d) mod s_count_in_day )

let duration_pp ppf (x : T.span) =
  let days, seconds = to_days_seconds x in
  let hours = seconds / 3600 in
  let minutes = seconds mod 3600 / 60 in
  let seconds = seconds mod 3600 mod 60 in
  fprintf ppf "D%dH%dM%dS%d" days hours minutes seconds

let datetime_pp (ppf : formatter) : datetime -> unit = function
  | UTCTimestamp d -> fprintf ppf "%a" timestamp_pp d
  | UTCTimeOnly d -> fprintf ppf "%a" timeonly_pp d
  | UTCDateOnly d -> fprintf ppf "%a" dateonly_pp d
  | LocalMktDate d -> fprintf ppf "%a" dateonly_pp d
  | MonthYear (d, w) -> fprintf ppf "%a" monthyear_pp (d, w)
  | Duration d -> fprintf ppf "%a" duration_pp d

let hof_type_pp (ppf : formatter) : hof_type -> unit = function
  | For_all -> fprintf ppf "forall"
  | Exists -> fprintf ppf "exists"
  | Map -> fprintf ppf "map"
  | Filter -> fprintf ppf "filter"
  | Find -> fprintf ppf "find"
  | For_all2 -> fprintf ppf "forall2"
  | Map2 -> fprintf ppf "map2"

let rec literal_pp (ppf : formatter) : literal -> unit = function
  | Bool true -> fprintf ppf "true"
  | Bool false -> fprintf ppf "false"
  | Int i -> fprintf ppf "%s" (Z.to_string i)
  | String s -> fprintf ppf "\"%s\"" (CCString.escaped s)
  | Float q -> fprintf ppf "%s" (Q.to_string q)
  | Coll (c, l) ->
    (match c with
    | Tuple ->
      fprintf ppf "(%a)" CCFormat.(list ~sep:(return ",") record_item_pp) l
    | Set ->
      fprintf ppf "{%a}" CCFormat.(list ~sep:(return ",") record_item_pp) l
    | List ->
      fprintf ppf "[%a]" CCFormat.(list ~sep:(return ",") record_item_pp) l)
  | MapColl (d, l) ->
    fprintf ppf "default:%a@, %a" record_item_pp d
      CCFormat.(list ~sep:(return " ") record_item_pair_pp)
      l
  | LiteralNone -> fprintf ppf "None"
  | LiteralSome e -> fprintf ppf "Some %a" record_item_pp e
  | Datetime d -> fprintf ppf "%a" datetime_pp d

and case_split_pair_pp (ppf : formatter) : record_item * record_item -> unit =
  function
  | check, value ->
    fprintf ppf "if (%a) : %a" record_item_pp check record_item_pp value

and value_pp (ppf : formatter) : value -> unit = function
  | Literal l -> literal_pp ppf l
  | LambdaVariable v | Variable v -> fprintf ppf "$%s" v
  | MessageValue mv -> fprintf ppf "%a" Message_value.pp mv
  | ObjectProperty op ->
    fprintf ppf "%a%a.%s" record_item_pp_parens op.obj opt_index_pp op.index
      op.prop
  | Funcall { func = Literal (String func); args } ->
    fprintf ppf "%s(%a)" func
      CCFormat.(list ~sep:(return ",") record_item_pp)
      args
  | Funcall { func; args } ->
    fprintf ppf "%a(%a)" value_pp func
      CCFormat.(list ~sep:(return ",") record_item_pp)
      args
  | CaseSplit { default_value; cases } ->
    fprintf ppf "%a"
      (pp_bullet_list
         (fun ppf x ->
           match x with
           | Rec_value (Value (Literal (String "default"))) ->
             CCFormat.fprintf ppf "%s" "default"
           | _ -> CCFormat.fprintf ppf "%a" record_item_pp x)
         record_item_pp)
      (cases @ [ Rec_value (Value (Literal (String "default"))), default_value ])
  | DataSetValue { name; field_name; default; constraints } ->
    fprintf ppf "dataset_value(%s,%s,%a) where [%a]" name field_name
      record_item_pp default
      CCFormat.(list ~sep:(return ",") record_item_pp)
      constraints
  | Hof { hof_type; lambda_args; body } ->
    fprintf ppf "(%a.{%a|%a})" hof_type_pp hof_type
      CCFormat.(list ~sep:(return " ") value_pp)
      lambda_args record_item_pp body

and opt_index_pp (ppf : formatter) : Z.t option -> unit = function
  | None -> ()
  | Some i -> fprintf ppf "[%s]" (Z.to_string i)

and expr_pp (ppf : formatter) : expr -> unit = function
  | Value v -> fprintf ppf "%a" value_pp v
  | Not e -> fprintf ppf "!%a" expr_pp_parens e
  | Or { lhs; rhs } ->
    fprintf ppf "%a || %a" expr_pp_parens lhs expr_pp_parens rhs
  | And { lhs; rhs } ->
    fprintf ppf "%a && %a" expr_pp_parens lhs expr_pp_parens rhs
  | Eq { lhs; rhs } ->
    fprintf ppf "%a = %a" record_item_pp_parens lhs record_item_pp_parens rhs
  | Cmp { lhs; op; rhs } ->
    fprintf ppf "%a %s %a" expr_pp_parens lhs op expr_pp_parens rhs
  | Add { lhs; op; rhs } ->
    fprintf ppf "%a %c %a" expr_pp_parens lhs op expr_pp_parens rhs
  | Mul { lhs; op; rhs } ->
    fprintf ppf "%a %c %a" expr_pp_parens lhs op expr_pp_parens rhs
  | In { el; set } -> fprintf ppf "%a in %a" expr_pp_parens el value_pp set

and record_item_pair_pp ppf (e1, e2) =
  fprintf ppf "%a -> %a" record_item_pp e1 record_item_pp e2

and expr_pp_parens ppf e =
  match e with
  | Value _ | Not _ -> expr_pp ppf e
  | Or _ | And _ | Eq _ | Cmp _ | Add _ | Mul _ | In _ ->
    fprintf ppf "(%a)" expr_pp e

and record_pp (ppf : formatter) (items : record) =
  let pp_item (ppf : formatter) ((name : string), (item : record_item)) =
    let open CCFormat in
    match item with
    | Rec_value _ -> fprintf ppf "%s = %a;" name record_item_pp item
    | Rec_record _ -> fprintf ppf "%s %a" name record_item_pp item
    | Rec_repeating_group _ -> record_item_pp ppf item
  in
  fprintf ppf "{@;<1 2>@[<v>%a@]@,}"
    CCFormat.(list ~sep:(return "@,") pp_item)
    (String_map.to_list items.elements |> CCList.rev)

and record_item_pp (ppf : formatter) (item : record_item) =
  let open CCFormat in
  match item with
  | Rec_value value -> expr_pp ppf value
  | Rec_record items -> record_pp ppf items
  | Rec_repeating_group { num_in_group_field; elements; _ } ->
    (match elements with
    | [] -> fprintf ppf "@[<v 2>%s@ [@[<v 1>@ {@,}@]@ ]@]" num_in_group_field
    | _ ->
      fprintf ppf "@[<v 2>%s@ [@[<v 1>@ %a@]@ ]@]" num_in_group_field
        (list ~sep:(return "@ ") record_pp)
        elements)

and record_item_pp_parens (ppf : formatter) (item : record_item) =
  let open CCFormat in
  match item with
  | Rec_value value -> expr_pp_parens ppf value
  | Rec_record items -> record_pp ppf items
  | Rec_repeating_group { num_in_group_field; elements; _ } ->
    (match elements with
    | [] -> fprintf ppf "@[<v 2>%s@ [@[<v 1>@ {@,}@]@ ]@]" num_in_group_field
    | _ ->
      fprintf ppf "@[<v 2>%s@ [@[<v 1>@ %a@]@ ]@]" num_in_group_field
        (list ~sep:(return "@ ") record_pp)
        elements)

let rec instruction_pp (ppf : formatter) : instruction -> unit =
  let open CCFormat in
  let pp_var_eq_opt fmt = function
    | None -> ()
    | Some var -> fprintf fmt "%s = " var
  in
  function
  | Action _ -> ()
  | Message _ -> ()
  | Set { prop; value } ->
    fprintf ppf "set %s = %a" prop record_item_pp_parens value
  | Send { variable; tag; withs; _ } ->
    fprintf ppf "@[<v>send %a(%s) %a@]" pp_var_eq_opt variable tag
      (CCFormat.some record_pp) withs 
  | Prompt { prop; and_set } ->
    fprintf ppf "@[<v>prompt %s %s@]"
      (if and_set then
        "prompt"
      else
        "prompt_and_set")
      prop
  | Receive { variable; where; expecting; _ } ->
    let pp_space_opt fmt = function
      | None -> ()
      | Some x -> fprintf fmt " %s" x
    in
    let pp_expecting_opt pp fmt = function
      | None -> ()
      | Some x -> fprintf fmt "@ expecting %a" pp x
    in
    fprintf ppf "@[<v 2>receive%a where %a%a@]" pp_space_opt variable expr_pp
      where
      (pp_expecting_opt expecting_pp)
      expecting
  | Comment comment ->
     fprintf ppf "@[<v 2>comment %s@]" comment

and expecting_pp fmt (expecting : expecting) =
  let open CCFormat in
  let es =
    expecting.relevant_exprs @ expecting.common_exprs
    @ expecting.qe_modified_exprs
    @ List.map fst expecting.nullable_exprs
  in
  fprintf fmt "{@,@[<v 2>  %a@]@,}" exprs_pp es

and exprs_pp fmt (exprs : expr list) =
  let open CCFormat in
  fprintf fmt "%a" (list ~sep:(return "@,") expr_pp) exprs

let instructions_pp = CCFormat.(vbox (list instruction_pp ~sep:(return "@,")))

let gen_pp filename =
  let open Decoders_yojson.Basic.Decode in
  let cwd = Sys.getcwd () in
  if String.length filename < 6 then
    failwith "not long enough to be a json file name"
  else (
    let json_ext = String.sub filename (String.length filename - 4) 4 in
    if json_ext <> "json" then
      failwith "Not a json extension"
    else (
      let base_filename = String.sub filename 0 (String.length filename - 5) in
      let instructions =
        decode_file
          (field "instructions" (list (Itr_ast_decoder.instruction_decoder ())))
          (CCFormat.sprintf "%s/%s" cwd filename)
      in
      match instructions with
      | Ok instructions ->
        let pp_string = CCFormat.sprintf "%a" instructions_pp instructions in
        let filename = CCFormat.sprintf "%s.out" base_filename in
        let oc = open_out filename in
        Printf.fprintf oc "%s\n" pp_string;
        close_out oc
      | Error e -> failwith (string_of_error e)
    )
  )
