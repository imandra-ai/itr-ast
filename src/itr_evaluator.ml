open Itr_ast

let retrieve_current_vars (expr : expr) : (string * Z.t option) list list =
  get_vars expr
  |> List.filter_map (function
       | Value (MessageValue { var = None; field_path }) -> Some field_path
       | _ -> None)

type 'a msg = {
  msg: 'a;
  tag: string;
}

type 'a msg_or_expr =
  | Msg of 'a msg
  | Record_item of record_item

type field_presence =
  | Present
  | NotPresent
  | Unknown

module Field_path_map = CCMap.Make (struct
  type t = field_path

  let compare = compare
end)

type 'a static_context = {
  local_vars: 'a msg_or_expr String_map.t;
  implicit_message: 'a msg option;
  get_field: 'a -> field_path -> record_item;
}

type field_presence_map = field_presence Field_path_map.t

type 'a context = {
  static_context: 'a static_context option;
  field_presence_map: field_presence_map;
}

let msg_or_expr_pp (ppf : Format.formatter) : 'a msg_or_expr -> unit =
  let open Format in
  function
  | Msg _m -> fprintf ppf "<msg>"
  | Record_item e -> fprintf ppf "%a" Itr_ast_pp.record_item_pp e

let failing_msg expr =
  let open CCFormat in
  fprintf str_formatter "%s"
    (Yojson.Basic.to_string @@ Itr_ast_json_pp.record_item_to_json expr);
  flush_str_formatter ()

let failing_pretty_msg expr =
  let open CCFormat in
  fprintf str_formatter "%a" Itr_ast_pp.record_item_pp expr;
  flush_str_formatter ()

let rec replace_in_expr e e_check e_replace =
  if e = e_check then
    e_replace
  else (
    match e with
    | Value (Funcall { func : string; args : record_item list }) ->
      Value
        (Funcall
           {
             func;
             args =
               List.map
                 (fun arg -> replace_in_record_item arg e_check e_replace)
                 args;
           })
    | Value _ -> e
    | Not expr -> Not (replace_in_expr expr e_check e_replace)
    | Or { lhs : expr; rhs : expr } ->
      Or
        {
          lhs = replace_in_expr lhs e_check e_replace;
          rhs = replace_in_expr rhs e_check e_replace;
        }
    | And { lhs : expr; rhs : expr } ->
      And
        {
          lhs = replace_in_expr lhs e_check e_replace;
          rhs = replace_in_expr rhs e_check e_replace;
        }
    | Eq { lhs : record_item; rhs : record_item } ->
      Eq
        {
          lhs = replace_in_record_item lhs e_check e_replace;
          rhs = replace_in_record_item rhs e_check e_replace;
        }
    | Cmp { lhs : expr; op : string; rhs : expr } ->
      Cmp
        {
          lhs = replace_in_expr lhs e_check e_replace;
          op;
          rhs = replace_in_expr rhs e_check e_replace;
        }
    | Add { lhs : expr; op : char; rhs : expr } ->
      Add
        {
          lhs = replace_in_expr lhs e_check e_replace;
          op;
          rhs = replace_in_expr rhs e_check e_replace;
        }
    | Mul { lhs : expr; op : char; rhs : expr } ->
      Mul
        {
          lhs = replace_in_expr lhs e_check e_replace;
          op;
          rhs = replace_in_expr rhs e_check e_replace;
        }
    | In { el : expr; set : value } ->
      In { el = replace_in_expr el e_check e_replace; set }
  )

and replace_in_record_item e e_check e_replace =
  match e with
  | Rec_value e -> Rec_value (replace_in_expr e e_check e_replace)
  | _ -> e

let e_true = Rec_value (Value (Literal (Bool true)))

let e_false = Rec_value (Value (Literal (Bool false)))

let e_none = Rec_value (Value (Literal LiteralNone))

let is_true = function
  | Rec_value (Value (Literal (Bool true))) -> true
  | _ -> false

let is_false = function
  | Rec_value (Value (Literal (Bool false))) -> true
  | _ -> false

let is_none = function
  | Rec_value (Value (Literal LiteralNone)) -> true
  | _ -> false

let check_cmp lhs rhs op =
  match op with
  | "<" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if l < r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if l < r then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.of_bigint l < r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if l < Q.of_bigint r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if Datetime.utctimestamp_LessThan_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if Datetime.utctimeonly_LessThan_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if Datetime.utcdateonly_LessThan l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if Datetime.localmktdate_LessThan l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear l))),
        Value (Literal (Datetime (MonthYear r))) ) ->
      if Datetime.monthyear_LessThan l r then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | ">" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if l > r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if l > r then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.of_bigint l > r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if l > Q.of_bigint r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if Datetime.utctimestamp_GreaterThan_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if Datetime.utctimeonly_GreaterThan_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if Datetime.utcdateonly_GreaterThan l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if Datetime.localmktdate_GreaterThan l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear l))),
        Value (Literal (Datetime (MonthYear r))) ) ->
      if Datetime.monthyear_GreaterThan l r then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | "<=" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if l <= r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if l <= r then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.of_bigint l <= r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if l <= Q.of_bigint r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if Datetime.utctimestamp_LessThanEqual_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if Datetime.utctimeonly_LessThanEqual_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if Datetime.utcdateonly_LessThanEqual l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if Datetime.localmktdate_LessThanEqual l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear l))),
        Value (Literal (Datetime (MonthYear r))) ) ->
      if Datetime.monthyear_LessThanEqual l r then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | ">=" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if l >= r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if l >= r then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.of_bigint l >= r then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if l >= Q.of_bigint r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if Datetime.utctimestamp_GreaterThanEqual_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if Datetime.utctimeonly_GreaterThanEqual_micro_micro l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if Datetime.utcdateonly_GreaterThanEqual l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if Datetime.localmktdate_GreaterThanEqual l r then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear l))),
        Value (Literal (Datetime (MonthYear r))) ) ->
      if Datetime.monthyear_GreaterThanEqual l r then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | _ -> Rec_value (Cmp { lhs; op; rhs })

let rec is_ground_expr : expr -> bool = function
  | Value (Variable _v) -> false
  | Value (MessageValue _mv) -> false
  | Value (Funcall { args; _ }) -> CCList.for_all is_ground args
  | Value (ObjectProperty { obj; _ }) -> is_ground obj
  | Value (CaseSplit { default_value; cases }) ->
    is_ground default_value
    && CCList.for_all (fun (a, b) -> is_ground a && is_ground b) cases
  | Value (DataSetValue { default; constraints; _ }) ->
    is_ground default && CCList.for_all is_ground constraints
  | Value (Literal _) -> true
  | Not e1 -> is_ground_expr e1
  | Or { lhs; rhs }
  | And { lhs; rhs }
  | Cmp { lhs; rhs; _ }
  | Add { lhs; rhs; _ }
  | Mul { lhs; rhs; _ } ->
    is_ground_expr lhs && is_ground_expr rhs
  | Eq { lhs; rhs } -> is_ground lhs && is_ground rhs
  | In { el; _ } -> is_ground_expr el

and is_ground (e : record_item) : bool =
  match e with
  | Rec_value r -> is_ground_expr r
  | Rec_record { elements; _ } ->
    String_map.for_all (fun _ x -> is_ground x) elements
  (* TODO checking of ground for repeating group structures *)
  | _ -> false

module Expr_set = CCSet.Make (struct
  type t = expr

  let compare = compare
end)

let rec get_assoc_ors (e : expr) : Expr_set.t =
  match e with
  | Or { lhs; rhs } -> Expr_set.union (get_assoc_ors lhs) (get_assoc_ors rhs)
  | _ -> Expr_set.singleton e

let rec get_assoc_ands (e : expr) : Expr_set.t =
  match e with
  | And { lhs; rhs } -> Expr_set.union (get_assoc_ands lhs) (get_assoc_ands rhs)
  | _ -> Expr_set.singleton e

let rec loem (es : record_item list) : record_item list =
  let rec remove (match_expr : expr) (ris : record_item list) (found : bool) :
      record_item list * bool =
    match ris with
    | [] -> [], found
    | Rec_value h :: t ->
      if h = Not match_expr || match_expr = Not h then (
        let res, found = remove match_expr t true in
        res, found
      ) else (
        let res, found = remove match_expr t found in
        res, found
      )
    | h :: t ->
      let res, found = remove match_expr t found in
      h :: res, found
  in
  match es with
  | [] -> []
  | [ _ ] -> es
  | Rec_value h :: t ->
    let rest, found = remove h t false in
    if found then
      e_true :: loem rest
    else
      Rec_value h :: loem t
  | h :: t -> h :: loem t

let reconstruct_ors (es : record_item list) : record_item =
  let rec reconstruct_ors_rec (es : record_item list) : expr =
    let expr_f = Value (Literal (Bool false)) in
    match es with
    | [] -> expr_f (* in theory not reachable *)
    | [ h ] ->
      (match h with
      | Rec_value h -> h
      | _ -> expr_f)
    | lhs :: t ->
      if CCList.mem lhs t then
        reconstruct_ors_rec t
      else (
        match lhs with
        | Rec_value lhs -> Or { lhs; rhs = reconstruct_ors_rec t }
        | _ -> expr_f
      )
  in
  Rec_value (reconstruct_ors_rec es)

let reconstruct_ands (es : record_item list) : record_item =
  let rec reconstruct_ands_rec (es : record_item list) : expr =
    let expr_f = Value (Literal (Bool false)) in
    match es with
    | [] -> expr_f (* in theory not reachable *)
    | [ h ] ->
      (match h with
      | Rec_value h -> h
      | _ -> expr_f)
    | lhs :: t ->
      if CCList.mem lhs t then
        reconstruct_ands_rec t
      else (
        match lhs with
        | Rec_value lhs -> And { lhs; rhs = reconstruct_ands_rec t }
        | _ -> expr_f
      )
  in
  Rec_value (reconstruct_ands_rec es)

let rec evaluate_record_item (context : 'a context) (e : record_item) :
    record_item =
  match e with
  | Rec_value r -> evaluate_expr context r
  | Rec_record { name; elements } ->
    Rec_record
      {
        name;
        elements = String_map.map (evaluate_record_item context) elements;
      }
  (* TODO simplification of repeating group structures *)
  | _ -> e

and evaluate_expr (context : 'a context) (e : expr) : record_item =
  let evaluate_expr = evaluate_expr context in
  let evaluate_record_item = evaluate_record_item context in
  match e with
  | Not (Not expr) -> evaluate_expr expr
  | Not (Value (Funcall { func : string; args : record_item list }))
    when func = "IsSet" && List.length args = 1 ->
    evaluate_expr
      (Eq
         { lhs = CCList.hd args; rhs = Rec_value (Value (Literal LiteralNone)) })
  | Value (CaseSplit { default_value; cases }) ->
    CCResult.fold_l
      (fun acc (check, value) ->
        if is_true (evaluate_record_item check) then
          Error (`GotResult (evaluate_record_item value))
        else
          Ok acc)
      (evaluate_record_item default_value)
      cases
    |> ( function
    | Ok res -> res
    | Error (`GotResult ri) -> ri )
  | Not expr ->
    (match evaluate_expr expr with
    | Rec_value (Value (Literal (Bool true))) ->
      Rec_value (Value (Literal (Bool false)))
    | Rec_value (Value (Literal (Bool false))) ->
      Rec_value (Value (Literal (Bool true)))
    | Rec_value e -> Rec_value (Not e)
    | _ -> Rec_value e)
  | Or { lhs : expr; rhs : expr } ->
    let ors =
      Expr_set.to_list @@ Expr_set.union (get_assoc_ors lhs) (get_assoc_ors rhs)
    in
    let evaluated_ors = CCList.map evaluate_expr ors in
    let evaluated_ors = loem evaluated_ors in
    if CCList.exists is_true evaluated_ors then
      e_true
    else if CCList.for_all is_false evaluated_ors then
      e_false
    else (
      let simplified_ors =
        CCList.filter (fun x -> not (is_false x)) evaluated_ors
      in
      let res = reconstruct_ors simplified_ors in
      res
    )
  | And { lhs : expr; rhs : expr } ->
    let ands =
      Expr_set.to_list
      @@ Expr_set.union (get_assoc_ands lhs) (get_assoc_ands rhs)
    in
    let evaluated_ands = CCList.map evaluate_expr ands in
    if CCList.exists is_false evaluated_ands then
      e_false
    else if CCList.for_all is_true evaluated_ands then
      e_true
    else (
      let simplified_ands =
        CCList.filter (fun x -> not (is_true x)) evaluated_ands
      in
      reconstruct_ands simplified_ands
    )
  | Eq { lhs : record_item; rhs : record_item } ->
    let lhs = evaluate_record_item lhs in
    let rhs = evaluate_record_item rhs in
    if lhs = rhs then
      e_true
    else if
      (* only evaluate equality to false if the terms have no variables *)
      is_ground lhs && is_ground rhs
    then
      e_false
    else
      Rec_value (Eq { lhs; rhs })
  | Cmp { lhs : expr; op : string; rhs : expr } ->
    let lhs = evaluate_expr lhs in
    let rhs = evaluate_expr rhs in
    (match lhs, rhs with
    | Rec_value lhs, Rec_value rhs -> check_cmp lhs rhs op
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "String.length" && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (String s))) ->
        Rec_value (Value (Literal (Int (Z.of_int (CCString.length s)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when (func = "String.length" || func = "LString.length")
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (String s))) ->
        Rec_value (Value (Literal (Int (Z.of_int (String.length s)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
  (* translate here *)
    when func = "IsSet" && List.length args = 1 ->
    (match args with
    | [ a ] ->
      evaluate_expr
        (Not (Eq { lhs = a; rhs = Rec_value (Value (Literal LiteralNone)) }))
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "defaultIfNotSet" && List.length args = 2 ->
    (match args with
    | [
     Rec_value (Value (MessageValue { var = None; field_path = field_path1 }));
     Rec_value (Value (MessageValue { var = None; field_path = field_path2 }));
    ] ->
      (match
         ( Field_path_map.get field_path1 context.field_presence_map,
           Field_path_map.get field_path2 context.field_presence_map )
       with
      | Some Present, _ ->
        Rec_value
          (Value (MessageValue { var = None; field_path = field_path1 }))
      | _, Some Present ->
        Rec_value
          (Value (MessageValue { var = None; field_path = field_path2 }))
      | _ ->
        Rec_value
          (Value (Funcall { func; args = CCList.map evaluate_record_item args })))
    | [ _d; Rec_value (Value (MessageValue { var = None; field_path })) ] ->
      (match Field_path_map.get field_path context.field_presence_map with
      | Some Present ->
        Rec_value (Value (MessageValue { var = None; field_path }))
      | _ ->
        Rec_value
          (Value (Funcall { func; args = CCList.map evaluate_record_item args })))
    | [ Rec_value (Value (MessageValue { var = None; field_path })); _ ] ->
      (match Field_path_map.get field_path context.field_presence_map with
      | Some Present ->
        Rec_value (Value (MessageValue { var = None; field_path }))
      | _ ->
        Rec_value
          (Value (Funcall { func; args = CCList.map evaluate_record_item args })))
    | [ d; a ] ->
      let a = evaluate_record_item a in
      (match a with
      | Rec_value (Value (Literal LiteralNone)) -> evaluate_record_item d
      | _ ->
        if is_ground a && (not @@ is_none a) then
          a
        else
          Rec_value
            (Value
               (Funcall { func; args = CCList.map evaluate_record_item args })))
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "Set.subset" && List.length args = 2 ->
    (match args with
    | [ l; r ] ->
      let l = evaluate_record_item l in
      let r = evaluate_record_item r in
      (match l, r with
      | ( Rec_value (Value (Literal (Coll l))),
          Rec_value (Value (Literal (Coll r))) ) ->
        if CCList.subset ~eq:(fun a b -> a = b) l r then
          e_true
        else
          e_false
      | _l, _r ->
        (* print_endline @@ (let j = Itr_ast_json_pp.expr_to_json l in Yojson.Basic.to_string j);
           print_endline @@ (let j = Itr_ast_json_pp.expr_to_json r in Yojson.Basic.to_string j); *)
        Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "FormatDate" ->
    (match args with
    | [ date; Rec_value (Value (Literal (String "yyyyMMdd"))) ] ->
      (match evaluate_record_item date with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value
             (Literal
                (Datetime
                   (UTCDateOnly
                      (Datetime.convert_utctimestamp_micro_utcdateonly t)))))
      | Rec_value (Value (MessageValue mv)) ->
        Rec_value
          (Value
             (Funcall
                {
                  func = "DayOf";
                  args = [ Rec_value (Value (MessageValue mv)) ];
                }))
      | _ -> Rec_value e)
    | [ date; Rec_value (Value (Literal (String "yyyyMMdd-HH:mm:ss.SSS"))) ] ->
      evaluate_record_item date
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "date" && args = [] ->
    Rec_value
      (Value
         (Literal
            (Datetime
               (UTCTimestamp (Current_time.get_current_utctimestamp_micro ())))))
  | Value (Funcall { func : string; args : record_item list })
    when func = "timestamp_to_dateonly" && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value
             (Literal
                (Datetime
                   (UTCDateOnly
                      (Datetime.convert_utctimestamp_micro_utcdateonly t)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "timestamp_to_localmktdate" && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value
             (Literal
                (Datetime
                   (LocalMktDate
                      (Datetime.convert_utctimestamp_micro_localmktdate t)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "randInt" && List.length args = 2 ->
    (match args with
    | [ l; u ] ->
      (match evaluate_record_item l, evaluate_record_item u with
      | Rec_value (Value (Literal (Int l))), Rec_value (Value (Literal (Int u)))
        ->
        let r = Z.(of_int (Random.int Z.(to_int (u - l))) + l) in
        Rec_value (Value (Literal (Int r)))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "Map.get_default" && List.length args = 1 ->
    (match args with
    | [ m ] ->
      (match evaluate_record_item m with
      | Rec_value (Value (Literal (MapColl (d, _)))) -> d
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "Map.get" && List.length args = 2 ->
    (match args with
    | [ m; k ] ->
      (match evaluate_record_item m, evaluate_record_item k with
      | Rec_value (Value (Literal (MapColl (d, vs)))), k ->
        (match CCList.find_opt (fun (a, _) -> a = k) vs with
        | None -> d
        | Some (_, v) -> v)
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : string; args : record_item list })
    when func = "Map.add" && List.length args = 3 ->
    (match args with
    | [ m; k; v ] ->
      (match
         evaluate_record_item m, evaluate_record_item k, evaluate_record_item v
       with
      | Rec_value (Value (Literal (MapColl (d, vs)))), k, v ->
        (match CCList.find_opt (fun (a, _) -> a = k) vs with
        | None -> Rec_value (Value (Literal (MapColl (d, (k, v) :: vs))))
        | Some p ->
          Rec_value
            (Value
               (Literal
                  (MapColl (d, (k, v) :: CCList.remove ~eq:( = ) ~key:p vs)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall _) -> Rec_value e
  | Value (Variable v) ->
    (match context.static_context with
    | None -> Rec_value e
    | Some context ->
      (match String_map.get v context.local_vars with
      | Some (Record_item ri) -> evaluate_record_item ri
      | _ -> Rec_value e))
  | Value (Literal (Coll l)) ->
    Rec_value (Value (Literal (Coll (List.map evaluate_record_item l))))
  | Value (Literal (MapColl (d, vs))) ->
    Rec_value
      (Value
         (Literal
            (MapColl
               ( evaluate_record_item d,
                 List.map
                   (fun (l, r) ->
                     evaluate_record_item l, evaluate_record_item r)
                   vs ))))
  | Value (Literal (LiteralSome s)) ->
    Rec_value (Value (Literal (LiteralSome (evaluate_record_item s))))
  | Value (MessageValue { var; field_path }) ->
    (match context.static_context with
    | None -> Rec_value e
    | Some context ->
      (match var, context.implicit_message with
      | Some x, _ ->
        (match String_map.get x context.local_vars with
        | Some (Msg { msg; _ }) ->
          evaluate_record_item (context.get_field msg field_path)
        | Some (Record_item ri) -> evaluate_record_item ri
        | _ -> Rec_value e)
      | None, Some { msg; _ } ->
        evaluate_record_item (context.get_field msg field_path)
      | None, None -> Rec_value e))
  | Add { lhs : expr; op : char; rhs : expr } ->
    let lhs, rhs = evaluate_expr lhs, evaluate_expr rhs in
    (match lhs, rhs with
    | Rec_value lhs, Rec_value rhs ->
      (match lhs, op, rhs with
      | Value (Literal (Int a)), '+', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Int Z.(a + b))))
      | Value (Literal (Float a)), '+', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(a + b))))
      | Value (Literal (Float a)), '+', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Float Q.(a + of_bigint b))))
      | Value (Literal (Int a)), '+', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(of_bigint a + b))))
      | Value (Literal (Int a)), '-', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Int Z.(a - b))))
      | Value (Literal (Float a)), '-', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(a - b))))
      | Value (Literal (Float a)), '-', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Float Q.(a - of_bigint b))))
      | Value (Literal (Int a)), '-', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(of_bigint a - b))))
      | Value (Literal (String a)), '+', Value (Literal (String b)) ->
        Rec_value (Value (Literal (String (a ^ b))))
      | Value (Literal (String a)), '+', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (String (a ^ Z.to_string b))))
      | Value (Literal (Int a)), '+', Value (Literal (String b)) ->
        Rec_value (Value (Literal (String (Z.to_string a ^ b))))
      | Value (Literal (String a)), '+', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (String (a ^ Q.to_string b))))
      | Value (Literal (Float a)), '+', Value (Literal (String b)) ->
        Rec_value (Value (Literal (String (Q.to_string a ^ b))))
      | _, '+', _ | _, '-', _ -> Rec_value e
      | _, _, _ -> failwith "Unknown Add operator")
    | _ -> Rec_value e)
  | Mul { lhs : expr; op : char; rhs : expr } ->
    let lhs, rhs = evaluate_expr lhs, evaluate_expr rhs in
    (match lhs, rhs with
    | Rec_value lhs, Rec_value rhs ->
      (match lhs, op, rhs with
      | Value (Literal (Int a)), '*', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Int Z.(a * b))))
      | Value (Literal (Float a)), '*', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(a * b))))
      | Value (Literal (Float a)), '*', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Float Q.(a * of_bigint b))))
      | Value (Literal (Int a)), '*', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(of_bigint a * b))))
      | Value (Literal (Int a)), '/', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Float Q.(of_bigint a / of_bigint b))))
      | Value (Literal (Float a)), '/', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(a / b))))
      | Value (Literal (Float a)), '/', Value (Literal (Int b)) ->
        Rec_value (Value (Literal (Float Q.(a / of_bigint b))))
      | Value (Literal (Int a)), '/', Value (Literal (Float b)) ->
        Rec_value (Value (Literal (Float Q.(of_bigint a / b))))
      | _, '*', _ | _, '/', _ -> Rec_value e
      | _, _, _ -> failwith "Unknown Mul operator")
    | _ -> Rec_value e)
  | In { el : expr; set : value } ->
    (match evaluate_expr (Value set) with
    | Rec_value (Value (Literal (Coll es))) ->
      Rec_value
        (Value
           (Literal
              (Bool
                 (List.mem (evaluate_expr el)
                    (List.map evaluate_record_item es)))))
    | _ -> Rec_value e)
  | _ -> Rec_value e

let fix_evaluate_record_item context record_item =
  let res = evaluate_record_item context record_item in
  if res = record_item then
    record_item
  else
    evaluate_record_item context res

let fix_evaluate_constraints context constraints =
  CCList.map (fix_evaluate_record_item context) constraints

let get_all_subsets (l : 'a list) : 'a list list =
  let rec get_all_subsets_tr (l : 'a list) (acc : 'a list list) : 'a list list =
    match l with
    | [] -> acc
    | h :: t ->
      let res = get_all_subsets_tr t [] in
      ([ h ] :: res) @ List.map (fun x -> h :: x) res
  in
  get_all_subsets_tr l []

let rec infer_field_presence (context : 'a context) (e : expr) : field_path list
    =
  let evaluate_record_item = evaluate_record_item context in
  let evaluate_expr = evaluate_expr context in
  match e with
  | Cmp
      {
        lhs =
          Value (Funcall { func = "defaultIfNotSet"; args = [ def_val; ri ] });
        op;
        rhs;
      } ->
    let def_val, ri = evaluate_record_item def_val, evaluate_record_item ri in
    if
      (is_ground def_val && is_ground ri)
      || ((not @@ is_ground def_val) && (not @@ is_ground ri))
    then
      []
    else (
      match def_val, ri with
      | ( Rec_value lhs,
          Rec_value (Value (MessageValue { var = None; field_path })) ) ->
        if is_false @@ evaluate_expr (Cmp { lhs; op; rhs }) then
          [ field_path ]
        else
          []
      | ( Rec_value (Value (MessageValue { var = None; field_path })),
          Rec_value ri ) ->
        if is_false @@ evaluate_expr (Cmp { lhs = ri; op; rhs }) then
          [ field_path ]
        else
          []
      | _ -> []
    )
  | Eq
      {
        lhs =
          Rec_value
            (Value
              (Funcall { func = "defaultIfNotSet"; args = [ def_val; ri ] }));
        rhs;
      } ->
    let def_val, ri = evaluate_record_item def_val, evaluate_record_item ri in
    if
      (is_ground def_val && is_ground ri)
      || ((not @@ is_ground def_val) && (not @@ is_ground ri))
    then
      []
    else (
      match def_val, ri with
      | lhs, Rec_value (Value (MessageValue { var = None; field_path })) ->
        if is_false @@ evaluate_expr (Eq { lhs; rhs }) then
          [ field_path ]
        else
          []
      | Rec_value (Value (MessageValue { var = None; field_path })), ri ->
        if is_false @@ evaluate_expr (Eq { lhs = ri; rhs }) then
          [ field_path ]
        else
          []
      | _ -> []
    )
  | And _ ->
    let assoc_ands = get_assoc_ands e in
    CCList.fold_left
      (fun acc el -> infer_field_presence context el @ acc)
      []
      (Expr_set.to_list assoc_ands)
  | _ -> []

(* return here set of sets -
   each set element is the set of field_paths which (and) have
    to be None concurrently to render the expression true

   the set of sets of these is the possible (or) combinations.

   e.g. (!IsSet(A) && !IsSet(B))||!IsSet(C)

   would give you {| {|A,B|} , {|C|} |}

   which is analogous to DNF
*)
let nullable_expr (expr : expr)
    (message_field_optional_map : bool String_map.t option)
    (field_presence_map : field_presence_map) : Field_path_set.t list =
  let open Itr_ast in
  let this_msg_vars =
    match message_field_optional_map with
    | None -> Field_path_set.empty
    | Some message_field_optional_map ->
      retrieve_current_vars expr
      |> List.filter (fun l ->
             let nm = String.concat "." (List.map fst l) in
             match String_map.get nm message_field_optional_map with
             | None -> false
             | Some is_optional -> is_optional)
      |> Field_path_set.of_list
  in
  let this_msg_vars = get_all_subsets (Field_path_set.to_list this_msg_vars) in
  CCList.fold_left
    (fun acc el ->
      let expr =
        List.fold_left
          (fun expr inner_el ->
            replace_in_expr expr
              (Value (MessageValue { var = None; field_path = inner_el }))
              (Value (Literal LiteralNone)))
          expr el
      in
      let record_item =
        fix_evaluate_record_item
          { static_context = None; field_presence_map }
          (Rec_value expr)
      in
      if is_true record_item then
        Field_path_set.of_list el :: acc
      else
        acc)
    [] this_msg_vars
