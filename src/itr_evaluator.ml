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

let empty_context =
  { static_context = None; field_presence_map = Field_path_map.empty }

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

let convert_to_dateonly (ts : T.t) : T.t =
  let d, _ps = ts |> T.to_span |> T.Span.to_d_ps in
  T.unsafe_of_d_ps (d, Z.zero)

let rec replace_expr_in_expr e e_check e_replace =
  if e = e_check then
    e_replace
  else (
    match e with
    | Value (Funcall { func : value; args : record_item list }) ->
      Value
        (Funcall
           {
             func;
             args =
               List.map
                 (fun arg ->
                   replace_record_item_in_record_item arg (Rec_value e_check)
                     (Rec_value e_replace))
                 args;
           })
    | Value
        (CaseSplit
          {
            default_value : record_item;
            cases : (record_item * record_item) list;
          }) ->
      Value
        (CaseSplit
           {
             default_value =
               replace_record_item_in_record_item default_value
                 (Rec_value e_check) (Rec_value e_replace);
             cases =
               CCList.map
                 (fun (c, s) ->
                   ( replace_record_item_in_record_item c (Rec_value e_check)
                       (Rec_value e_replace),
                     replace_record_item_in_record_item s (Rec_value e_check)
                       (Rec_value e_replace) ))
                 cases;
           })
    | Value
        (ObjectProperty
          { obj : record_item; index : Z.t option; prop : string }) ->
      Value
        (ObjectProperty
           {
             obj =
               replace_record_item_in_record_item obj (Rec_value e_check)
                 (Rec_value e_replace);
             index;
             prop;
           })
    | Value (Literal (Coll (ct, ri))) ->
      Value
        (Literal
           (Coll
              ( ct,
                CCList.map
                  (fun x ->
                    replace_record_item_in_record_item x (Rec_value e_check)
                      (Rec_value e_replace))
                  ri )))
    | Value (Literal (MapColl (def, ris))) ->
      Value
        (Literal
           (MapColl
              ( replace_record_item_in_record_item def (Rec_value e_check)
                  (Rec_value e_replace),
                CCList.map
                  (fun (c, s) ->
                    ( replace_record_item_in_record_item c (Rec_value e_check)
                        (Rec_value e_replace),
                      replace_record_item_in_record_item s (Rec_value e_check)
                        (Rec_value e_replace) ))
                  ris )))
    | Value (Literal (LiteralSome ri)) ->
      Value
        (Literal
           (LiteralSome
              (replace_record_item_in_record_item ri (Rec_value e_check)
                 (Rec_value e_replace))))
    | Value
        (Hof
          { hof_type : hof_type; lambda_args : value list; body : record_item })
      ->
      Value
        (Hof
           {
             hof_type;
             lambda_args;
             body =
               replace_record_item_in_record_item body (Rec_value e_check)
                 (Rec_value e_replace);
           })
    | Value _ -> e
    | Not expr -> Not (replace_expr_in_expr expr e_check e_replace)
    | Or { lhs : expr; rhs : expr } ->
      Or
        {
          lhs = replace_expr_in_expr lhs e_check e_replace;
          rhs = replace_expr_in_expr rhs e_check e_replace;
        }
    | And { lhs : expr; rhs : expr } ->
      And
        {
          lhs = replace_expr_in_expr lhs e_check e_replace;
          rhs = replace_expr_in_expr rhs e_check e_replace;
        }
    | Eq { lhs : record_item; rhs : record_item } ->
      Eq
        {
          lhs =
            replace_record_item_in_record_item lhs (Rec_value e_check)
              (Rec_value e_replace);
          rhs =
            replace_record_item_in_record_item rhs (Rec_value e_check)
              (Rec_value e_replace);
        }
    | Cmp { lhs : expr; op : string; rhs : expr } ->
      Cmp
        {
          lhs = replace_expr_in_expr lhs e_check e_replace;
          op;
          rhs = replace_expr_in_expr rhs e_check e_replace;
        }
    | Add { lhs : expr; op : char; rhs : expr } ->
      Add
        {
          lhs = replace_expr_in_expr lhs e_check e_replace;
          op;
          rhs = replace_expr_in_expr rhs e_check e_replace;
        }
    | Mul { lhs : expr; op : char; rhs : expr } ->
      Mul
        {
          lhs = replace_expr_in_expr lhs e_check e_replace;
          op;
          rhs = replace_expr_in_expr rhs e_check e_replace;
        }
    | In { el : expr; set : value } ->
      In { el = replace_expr_in_expr el e_check e_replace; set }
  )

and replace_record_item_in_expr (ri : expr) (e_check : record_item)
    (e_replace : record_item) =
  match ri with
  | Value (Funcall { func : value; args : record_item list }) ->
    Rec_value
      (Value
         (Funcall
            {
              func;
              args =
                List.map
                  (fun arg ->
                    replace_record_item_in_record_item arg e_check e_replace)
                  args;
            }))
  | Eq { lhs : record_item; rhs : record_item } ->
    Rec_value
      (Eq
         {
           lhs = replace_record_item_in_record_item lhs e_check e_replace;
           rhs = replace_record_item_in_record_item rhs e_check e_replace;
         })
  | Value
      (ObjectProperty { obj : record_item; index : Z.t option; prop : string })
    ->
    Rec_value
      (Value
         (ObjectProperty
            {
              obj = replace_record_item_in_record_item obj e_check e_replace;
              index;
              prop;
            }))
  | Value
      (CaseSplit
        {
          default_value : record_item;
          cases : (record_item * record_item) list;
        }) ->
    Rec_value
      (Value
         (CaseSplit
            {
              default_value =
                replace_record_item_in_record_item default_value e_check
                  e_replace;
              cases =
                CCList.map
                  (fun (c, s) ->
                    ( replace_record_item_in_record_item c e_check e_replace,
                      replace_record_item_in_record_item s e_check e_replace ))
                  cases;
            }))
  | Value
      (Hof
        { hof_type : hof_type; lambda_args : value list; body : record_item })
    ->
    Rec_value
      (Value
         (Hof
            {
              hof_type;
              lambda_args;
              body = replace_record_item_in_record_item body e_check e_replace;
            }))
  | Value (Literal (Coll (ct, ri))) ->
    Rec_value
      (Value
         (Literal
            (Coll
               ( ct,
                 CCList.map
                   (fun x ->
                     replace_record_item_in_record_item x e_check e_replace)
                   ri ))))
  | Value (Literal (MapColl (def, ris))) ->
    Rec_value
      (Value
         (Literal
            (MapColl
               ( replace_record_item_in_record_item def e_check e_replace,
                 CCList.map
                   (fun (c, s) ->
                     ( replace_record_item_in_record_item c e_check e_replace,
                       replace_record_item_in_record_item s e_check e_replace ))
                   ris ))))
  | Value (Literal (LiteralSome ri)) ->
    Rec_value
      (Value
         (Literal
            (LiteralSome
               (replace_record_item_in_record_item ri e_check e_replace))))
  | Not expr ->
    (match replace_record_item_in_expr expr e_check e_replace with
    | Rec_value expr -> Rec_value (Not expr)
    | _ -> Rec_value (Not expr))
  | Or { lhs : expr; rhs : expr } ->
    (match
       ( replace_record_item_in_expr lhs e_check e_replace,
         replace_record_item_in_expr rhs e_check e_replace )
     with
    | Rec_value lhs, Rec_value rhs -> Rec_value (Or { lhs; rhs })
    | _ -> Rec_value (Or { lhs; rhs }))
  | And { lhs : expr; rhs : expr } ->
    (match
       ( replace_record_item_in_expr lhs e_check e_replace,
         replace_record_item_in_expr rhs e_check e_replace )
     with
    | Rec_value lhs, Rec_value rhs -> Rec_value (And { lhs; rhs })
    | _ -> Rec_value (And { lhs; rhs }))
  | Cmp { lhs : expr; op : string; rhs : expr } ->
    (match
       ( replace_record_item_in_expr lhs e_check e_replace,
         replace_record_item_in_expr rhs e_check e_replace )
     with
    | Rec_value lhs, Rec_value rhs -> Rec_value (Cmp { lhs; op; rhs })
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | Add { lhs : expr; op : char; rhs : expr } ->
    (match
       ( replace_record_item_in_expr lhs e_check e_replace,
         replace_record_item_in_expr rhs e_check e_replace )
     with
    | Rec_value lhs, Rec_value rhs -> Rec_value (Add { lhs; op; rhs })
    | _ -> Rec_value (Add { lhs; op; rhs }))
  | Mul { lhs : expr; op : char; rhs : expr } ->
    (match
       ( replace_record_item_in_expr lhs e_check e_replace,
         replace_record_item_in_expr rhs e_check e_replace )
     with
    | Rec_value lhs, Rec_value rhs -> Rec_value (Mul { lhs; op; rhs })
    | _ -> Rec_value (Mul { lhs; op; rhs }))
  | In { el : expr; set : value } ->
    (match replace_record_item_in_expr el e_check e_replace with
    | Rec_value el -> Rec_value (In { el; set })
    | _ -> Rec_value (In { el; set }))
  | ri ->
    (match e_check, e_replace with
    | Rec_value e_check, Rec_value e_replace ->
      Rec_value (replace_expr_in_expr ri e_check e_replace)
    | _ -> Rec_value ri)

and replace_expr_in_record_item ri (e_check : expr) (e_replace : expr) =
  match ri with
  | Rec_value e -> Rec_value (replace_expr_in_expr e e_check e_replace)
  | Rec_record { name; elements } ->
    Rec_record
      {
        name;
        elements =
          String_map.map
            (fun e -> replace_expr_in_record_item e e_check e_replace)
            elements;
      }
  | Rec_repeating_group
      {
        name : string;
        message_template : string option;
        num_in_group_field : string;
        elements : record list;
      } ->
    Rec_repeating_group
      {
        name;
        message_template;
        num_in_group_field;
        elements =
          CCList.map
            (fun { name; elements } ->
              {
                name;
                elements =
                  String_map.map
                    (fun e -> replace_expr_in_record_item e e_check e_replace)
                    elements;
              })
            elements;
      }

(* replace : ri -> check -> replace -> ri *)
and replace_record_item_in_record_item ri (e_check : record_item)
    (e_replace : record_item) =
  if ri = e_check then
    e_replace
  else (
    match ri with
    | Rec_value ri -> replace_record_item_in_expr ri e_check e_replace
    | Rec_record { name; elements } ->
      Rec_record
        {
          name;
          elements =
            String_map.map
              (fun e -> replace_record_item_in_record_item e e_check e_replace)
              elements;
        }
    | Rec_repeating_group
        {
          name : string;
          message_template : string option;
          num_in_group_field : string;
          elements : record list;
        } ->
      Rec_repeating_group
        {
          name;
          message_template;
          num_in_group_field;
          elements =
            CCList.map
              (fun { name; elements } ->
                {
                  name;
                  elements =
                    String_map.map
                      (fun e ->
                        replace_record_item_in_record_item e e_check e_replace)
                      elements;
                })
              elements;
        }
  )

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
      if Z.(l < r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if Q.(l < r) then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.(of_bigint l < r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if Q.(l < of_bigint r) then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if T.compare l r < Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if T.compare l r < Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if T.compare l r < Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if T.compare l r < Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear (l, _w1)))),
        Value (Literal (Datetime (MonthYear (r, _w2)))) ) ->
      if T.compare l r < Z.zero then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | ">" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if Z.(l > r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if Q.(l > r) then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.(of_bigint l > r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if Q.(l > of_bigint r) then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if T.compare l r > Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if T.compare l r > Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if T.compare l r > Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if T.compare l r > Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear (l, _w1)))),
        Value (Literal (Datetime (MonthYear (r, _w2)))) ) ->
      if T.compare l r > Z.zero then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | "<=" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if Z.(l <= r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if Q.(l <= r) then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.(of_bigint l <= r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if Q.(l <= of_bigint r) then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if T.compare l r <= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if T.compare l r <= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if T.compare l r <= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if T.compare l r <= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear (l, _w1)))),
        Value (Literal (Datetime (MonthYear (r, _w2)))) ) ->
      if T.compare l r <= Z.zero then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | ">=" ->
    (match lhs, rhs with
    | Value (Literal (Int l)), Value (Literal (Int r)) ->
      if Z.(l >= r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Float r)) ->
      if Q.(l >= r) then
        e_true
      else
        e_false
    | Value (Literal (Int l)), Value (Literal (Float r)) ->
      if Q.(of_bigint l >= r) then
        e_true
      else
        e_false
    | Value (Literal (Float l)), Value (Literal (Int r)) ->
      if Q.(l >= of_bigint r) then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimestamp l))),
        Value (Literal (Datetime (UTCTimestamp r))) ) ->
      if T.compare l r >= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCTimeOnly l))),
        Value (Literal (Datetime (UTCTimeOnly r))) ) ->
      if T.compare l r >= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (UTCDateOnly l))),
        Value (Literal (Datetime (UTCDateOnly r))) ) ->
      if T.compare l r >= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (LocalMktDate l))),
        Value (Literal (Datetime (LocalMktDate r))) ) ->
      if T.compare l r >= Z.zero then
        e_true
      else
        e_false
    | ( Value (Literal (Datetime (MonthYear (l, _w1)))),
        Value (Literal (Datetime (MonthYear (r, _w2)))) ) ->
      if T.compare l r >= Z.zero then
        e_true
      else
        e_false
    | _ -> Rec_value (Cmp { lhs; op; rhs }))
  | _ -> Rec_value (Cmp { lhs; op; rhs })

let rec no_free_vars_expr (bound_lambda_vars : String_set.t) : expr -> bool =
  let is_ground_expr x = no_free_vars_expr bound_lambda_vars x in
  let is_ground x = no_free_vars bound_lambda_vars x in
  function
  | Value (Variable _v) -> false
  | Value (MessageValue _mv) -> false
  | Value (LambdaVariable v) -> String_set.mem v bound_lambda_vars
  | Value (Funcall { args; _ }) -> CCList.for_all is_ground args
  | Value (ObjectProperty { obj; _ }) -> is_ground obj
  | Value (Hof { body; lambda_args; _ }) ->
    let bound_lambda_vars =
      String_set.add_list bound_lambda_vars
        (lambda_args
        |> List.filter_map (function
             | LambdaVariable v -> Some v
             | _ -> None))
    in
    no_free_vars bound_lambda_vars body
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

and no_free_vars (bound_lambda_vars : String_set.t) (e : record_item) : bool =
  let is_ground x = no_free_vars bound_lambda_vars x in
  match e with
  | Rec_value r -> no_free_vars_expr bound_lambda_vars r
  | Rec_record { elements; _ } ->
    String_map.for_all (fun _ x -> is_ground x) elements
  | Rec_repeating_group { elements; _ } ->
    CCList.for_all (fun x -> is_ground (Rec_record x)) elements

let is_ground (e : record_item) : bool = no_free_vars String_set.empty e

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

let rec eval_add_datetime dt1 op dt2 =
  let default =
    Rec_value
      (Add
         {
           rhs = Value (Literal (Datetime dt1));
           op;
           lhs = Value (Literal (Datetime dt2));
         })
  in
  match dt2 with
  | Duration dur ->
    (match dt1 with
    | UTCTimestamp t ->
      let ts =
        match op with
        | '+' -> T.add_span t dur |> CCOption.get_or ~default:t
        | '-' ->
          T.add_span t (Imandra_ptime.Span.neg dur)
          |> CCOption.get_or ~default:t
        | _ -> failwith "Unknown Add operator"
      in
      Rec_value (Value (Literal (Datetime (UTCTimestamp ts))))
    | _ -> default)
  | _ ->
    (match op, dt1 with
    | '+', Duration _ -> eval_add_datetime dt2 op dt1
    | _ -> default)

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
  | Rec_repeating_group { name; elements; num_in_group_field; message_template }
    ->
    Rec_repeating_group
      {
        name;
        elements =
          CCList.map
            (fun { name; elements } ->
              {
                name;
                elements =
                  String_map.map (evaluate_record_item context) elements;
              })
            elements;
        num_in_group_field;
        message_template;
      }

and evaluate_expr (context : 'a context) (e : expr) : record_item =
  let evaluate_expr = evaluate_expr context in
  let evaluate_record_item = fix_evaluate_record_item context in
  match e with
  | Not (Not expr) -> evaluate_expr expr
  | Not (Value (Funcall { func : value; args : record_item list }))
    when func = Literal (String "IsSet") && List.length args = 1 ->
    evaluate_expr
      (Eq
         {
           lhs = evaluate_record_item (CCList.hd args);
           rhs = Rec_value (Value (Literal LiteralNone));
         })
  | Value (CaseSplit { default_value; cases }) ->
    let default_value = evaluate_record_item default_value in
    let cases =
      CCList.map
        (fun (k, v) -> evaluate_record_item k, evaluate_record_item v)
        cases
    in
    let simplified_input =
      Rec_value (Value (CaseSplit { default_value; cases }))
    in
    let case_result =
      CCResult.fold_l
        (fun acc (check, value) ->
          if is_true check then
            Error (`GotResult value)
          else
            Ok acc)
        default_value cases
    in
    (match case_result with
    | Ok res ->
      if CCList.for_all is_false (CCList.map fst cases) then
        res
      else
        cases |> CCList.filter (fun (_condition, value) -> not (is_false value))
        |> ( function
        (* Single case which could possibly evaluate to true, equivalent to just condition && value *)
        | [ (Rec_value condition, Rec_value value) ] when is_false default_value
          ->
          Rec_value (And { lhs = condition; rhs = value })
        | _ -> simplified_input )
    | Error (`GotResult ri) -> ri)
  | Value
      (ObjectProperty { obj : record_item; index : Z.t option; prop : string })
    ->
    (match evaluate_record_item obj with
    | Rec_record { elements; _ } as r ->
      (match index with
      | None ->
        (match String_map.get prop elements with
        | None -> r
        | Some e -> e)
      | _ -> r)
    | Rec_repeating_group { elements; _ } as r ->
      (match index with
      | Some i ->
        (try
           let rg_element = CCList.nth elements (Z.to_int i) in
           match String_map.get prop rg_element.elements with
           | None -> r
           | Some e -> e
         with _ -> r)
      | _ -> r)
    | _ -> Rec_value e)
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
  | Eq
      {
        lhs =
          Rec_value
            (Value
              (Funcall
                {
                  func =
                    Hof
                      {
                        hof_type = Map;
                        lambda_args = [ LambdaVariable x1 ];
                        body;
                      };
                  args =
                    [
                      Rec_value
                        (Value
                          (Funcall
                            {
                              func = Literal (String "take");
                              args =
                                [ Rec_value (Value (Literal (Int value))); arr ];
                            }));
                    ];
                }));
        rhs = Rec_value (Value (Literal (Coll (List, [ a ]))));
      }
    when Z.equal value Z.one ->
    (* Simplifies `map(f) take(1, arr) = [a]` to `f(hd(arr)) = a` *)
    let arg = Funcall { func = Literal (String "hd"); args = [ arr ] } in
    let lhs =
      replace_expr_in_record_item body (Value (LambdaVariable x1)) (Value arg)
    in
    let rhs = a in
    evaluate_expr (Eq { lhs; rhs })
  | Eq { lhs : record_item; rhs : record_item } ->
    let lhs = evaluate_record_item lhs in
    let rhs = evaluate_record_item rhs in
    if Itr_ast.equal_record_item lhs rhs then
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
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "len") && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (String s))) ->
        Rec_value (Value (Literal (Int (Z.of_int (CCString.length s)))))
      | Rec_value (Value (Literal (Coll (_, l)))) ->
        Rec_value (Value (Literal (Int (Z.of_int (CCList.length l)))))
      | Rec_repeating_group { elements; _ } ->
        Rec_value (Value (Literal (Int (Z.of_int (CCList.length elements)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "LString.length")
         || func = Literal (String "String.length"))
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (String s))) ->
        Rec_value (Value (Literal (Int (Z.of_int (CCString.length s)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "List.length") && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Coll (_, l)))) ->
        Rec_value (Value (Literal (Int (Z.of_int (CCList.length l)))))
      | Rec_repeating_group { elements; _ } ->
        Rec_value (Value (Literal (Int (Z.of_int (CCList.length elements)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "List.take") || func = Literal (String "take"))
         && List.length args = 2 ->
    (match args with
    | [ n; a ] ->
      (match evaluate_record_item n, evaluate_record_item a with
      | ( Rec_value (Value (Literal (Int n))),
          Rec_value (Value (Literal (Coll (ct, l)))) ) ->
        Rec_value (Value (Literal (Coll (ct, CCList.take (Z.to_int n) l))))
      | ( Rec_value (Value (Literal (Int n))),
          Rec_repeating_group
            { elements; message_template; num_in_group_field; name } ) ->
        Rec_repeating_group
          {
            elements = CCList.take (Z.to_int n) elements;
            message_template;
            num_in_group_field;
            name;
          }
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "String.length")
         || func = Literal (String "LString.length")
         || func = Literal (String "len"))
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (String s))) ->
        Rec_value (Value (Literal (Int (Z.of_int (String.length s)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "List.hd") || func = Literal (String "hd"))
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Coll (List, l)))) ->
        (match l with
        | [] -> Rec_value e
        | h :: _t -> h)
      | Rec_repeating_group { elements = h :: _; _ } -> Rec_record h
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "List.tl") || func = Literal (String "tl"))
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Coll (List, l)))) ->
        (match l with
        | [] -> Rec_value e
        | _ :: t -> Rec_value (Value (Literal (Coll (List, t)))))
      | Rec_repeating_group
          { elements = _ :: t; name; message_template; num_in_group_field } ->
        Rec_repeating_group
          { elements = t; name; message_template; num_in_group_field }
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "IsSet") && List.length args = 1 ->
    (match args with
    (* Simplifies IsSet(Find X) to (Exists X) *)
    | [
     Rec_value
       (Value
         (Funcall { func = Hof { hof_type = Find; lambda_args; body }; args }));
    ] ->
      evaluate_expr
        (Value
           (Funcall
              { func = Hof { hof_type = Exists; lambda_args; body }; args }))
    | [ a ] ->
      evaluate_expr
        (Not (Eq { lhs = a; rhs = Rec_value (Value (Literal LiteralNone)) }))
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "implies") && List.length args = 2 ->
    (match CCList.map evaluate_record_item args with
    | [ Rec_value lhs; Rec_value rhs ] ->
      evaluate_expr (Or { lhs = Not lhs; rhs })
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "defaultIfNotSet") && List.length args = 2 ->
    (match args with
    | [
     Rec_value (Value (MessageValue { var = None; field_path = field_path1 }));
     Rec_value (Value (MessageValue { var = None; field_path = field_path2 }));
    ] ->
      (match
         ( Field_path_map.get field_path1 context.field_presence_map,
           Field_path_map.get field_path2 context.field_presence_map )
       with
      | _, Some Present ->
        Rec_value
          (Value (MessageValue { var = None; field_path = field_path2 }))
      | Some Present, _ ->
        Rec_value
          (Value (MessageValue { var = None; field_path = field_path1 }))
      | _ ->
        Rec_value
          (Value (Funcall { func; args = CCList.map evaluate_record_item args })))
    | [ d; Rec_value (Value (MessageValue { var = None; field_path }) as a) ] ->
      (match Field_path_map.get field_path context.field_presence_map with
      | Some Present ->
        Rec_value (Value (MessageValue { var = None; field_path }))
      | _ ->
        let a = evaluate_expr a in
        (match a with
        | Rec_value (Value (Literal LiteralNone)) -> evaluate_record_item d
        | _ ->
          if is_ground a then
            a
          else
            Rec_value
              (Value
                 (Funcall { func; args = CCList.map evaluate_record_item args }))))
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
        if is_ground a then
          a
        else
          Rec_value
            (Value
               (Funcall { func; args = CCList.map evaluate_record_item args })))
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when (func = Literal (String "Set.subset")
         || func = Literal (String "subset"))
         && List.length args = 2 ->
    (match args with
    | [ l; r ] ->
      let l = evaluate_record_item l in
      let r = evaluate_record_item r in
      (match l, r with
      | ( Rec_value (Value (Literal (Coll (_, l)))),
          Rec_value (Value (Literal (Coll (_, r)))) ) ->
        if CCList.subset ~eq:(fun a b -> a = b) l r then
          e_true
        else
          e_false
      | _l, _r -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "FormatDate") ->
    (match args with
    | [ date; Rec_value (Value (Literal (String "yyyyMMdd"))) ] ->
      (match evaluate_record_item date with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value (Literal (Datetime (UTCDateOnly (convert_to_dateonly t)))))
      | Rec_value (Value (MessageValue mv)) ->
        Rec_value
          (Value
             (Funcall
                {
                  func = Literal (String "DayOf");
                  args = [ Rec_value (Value (MessageValue mv)) ];
                }))
      | _ -> Rec_value e)
    | [ date; Rec_value (Value (Literal (String "yyyyMMdd-HH:mm:ss.SSS"))) ] ->
      evaluate_record_item date
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "date") && args = [] ->
    Rec_value
      (Value
         (Literal (Datetime (UTCTimestamp (Ptime_clock.now () |> T.of_ptime)))))
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "timestamp_to_dateonly") && List.length args = 1
    ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value (Literal (Datetime (UTCDateOnly (convert_to_dateonly t)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "timestamp_to_localmktdate")
         && List.length args = 1 ->
    (match args with
    | [ a ] ->
      (match evaluate_record_item a with
      | Rec_value (Value (Literal (Datetime (UTCTimestamp t)))) ->
        Rec_value
          (Value (Literal (Datetime (LocalMktDate (convert_to_dateonly t)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "randInt") && List.length args = 2 ->
    (match args with
    | [ l; u ] ->
      (match evaluate_record_item l, evaluate_record_item u with
      | Rec_value (Value (Literal (Int l))), Rec_value (Value (Literal (Int u)))
        ->
        let r = Z.(of_int (Random.int Z.(to_int (u - l))) + l) in
        Rec_value (Value (Literal (Int r)))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "randString") ->
    (match args with
    | l :: _ ->
      (match evaluate_record_item l with
      | Rec_value (Value (Literal (Int l))) ->
        let rec app_string l =
          if l <= 0 then
            ""
          else
            CCFormat.sprintf "%s%x" (app_string (l - 1)) (Random.int 15)
        in
        Rec_value (Value (Literal (String (app_string (Z.to_int l)))))
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "Map.get_default") && List.length args = 1 ->
    (match args with
    | [ m ] ->
      (match evaluate_record_item m with
      | Rec_value (Value (Literal (MapColl (d, _)))) -> d
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "Map.get") && List.length args = 2 ->
    (match args with
    | [ m; k ] ->
      (match evaluate_record_item m, evaluate_record_item k with
      | Rec_value (Value (Literal (MapColl (d, vs)))), k ->
        (match CCList.find_opt (fun (a, _) -> a = k) vs with
        | None -> d
        | Some (_, v) -> v)
      | _ -> Rec_value e)
    | _ -> Rec_value e)
  | Value (Funcall { func : value; args : record_item list })
    when func = Literal (String "Map.add") && List.length args = 3 ->
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
  | Value (Variable v) ->
    (match context.static_context with
    | None -> Rec_value e
    | Some context ->
      (match String_map.get v context.local_vars with
      | Some (Record_item ri) -> evaluate_record_item ri
      | _ -> Rec_value e))
  | Value (Literal (Coll (ct, l))) ->
    Rec_value (Value (Literal (Coll (ct, List.map evaluate_record_item l))))
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
  | Value (Literal (LiteralSome s)) -> evaluate_record_item s
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
  | Value
      (Funcall
        { func = Hof { hof_type; lambda_args; body }; args = [ arg1; arg2 ] })
    ->
    let grab_record_list = function
      | Rec_value (Value (Literal (Coll (_ct, args1)))) -> args1
      | Rec_repeating_group { elements; _ } ->
        CCList.map (fun r -> Rec_record r) elements
      (* not possible so a dummy return here *)
      | _ -> [ Rec_value (Value (Literal LiteralNone)) ]
    in
    let record1, record2 =
      ( grab_record_list (evaluate_record_item arg1),
        grab_record_list (evaluate_record_item arg2) )
    in
    (match hof_type with
    | For_all2 ->
      (match lambda_args with
      | [ LambdaVariable e_check1; LambdaVariable e_check2 ] ->
        evaluate_expr
          (CCList.fold_left2
             (fun rhs e_replace1 e_replace2 ->
               let repl =
                 evaluate_record_item
                   (replace_record_item_in_record_item body
                      (Rec_value (Value (LambdaVariable e_check1))) e_replace1)
               in
               let repl =
                 evaluate_record_item
                   (replace_record_item_in_record_item repl
                      (Rec_value (Value (LambdaVariable e_check2))) e_replace2)
               in
               match repl with
               | Rec_value lhs ->
                 (match evaluate_expr (And { lhs; rhs }) with
                 | Rec_value expr -> expr
                 | _ -> Value (Literal (Bool false)))
               | _ -> Value (Literal (Bool false)))
             (Value (Literal (Bool true))) record1 record2)
      | _ -> Rec_value e)
    | Map2 ->
      (match lambda_args with
      | [ LambdaVariable e_check1; LambdaVariable e_check2 ] ->
        Rec_value
          (Value
             (Literal
                (Coll
                   ( List,
                     CCList.map2
                       (fun e_replace1 e_replace2 ->
                         let repl =
                           evaluate_record_item
                             (replace_record_item_in_record_item body
                                (Rec_value (Value (LambdaVariable e_check1)))
                                e_replace1)
                         in
                         evaluate_record_item
                           (replace_record_item_in_record_item repl
                              (Rec_value (Value (LambdaVariable e_check2)))
                              e_replace2))
                       record1 record2 ))))
      | _ -> Rec_value e)
    | _ ->
      let body = evaluate_record_item body in
      Rec_value
        (Value
           (Funcall
              {
                func = Hof { hof_type; lambda_args; body };
                args = [ arg1; arg2 ];
              })))
  | Value (Funcall { func = Hof { hof_type; lambda_args; body }; args })
    when CCList.length args = 1 ->
    (match evaluate_record_item (CCList.hd args) with
    | Rec_value (Value (Literal (Coll (ct, args)))) ->
      (match hof_type with
      | For_all ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          evaluate_expr
            (CCList.fold_left
               (fun rhs e_replace ->
                 let repl =
                   evaluate_record_item
                     (replace_record_item_in_record_item body
                        (Rec_value (Value (LambdaVariable e_check))) e_replace)
                 in
                 match repl with
                 | Rec_value lhs ->
                   (match evaluate_expr (And { lhs; rhs }) with
                   | Rec_value expr -> expr
                   | _ -> Value (Literal (Bool false)))
                 | _ -> Value (Literal (Bool false)))
               (Value (Literal (Bool true))) args)
        | _ -> Rec_value e)
      | Exists ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          evaluate_expr
            (CCList.fold_left
               (fun rhs e_replace ->
                 let repl =
                   evaluate_record_item
                     (replace_record_item_in_record_item body
                        (Rec_value (Value (LambdaVariable e_check))) e_replace)
                 in
                 match repl with
                 | Rec_value lhs ->
                   (match evaluate_expr (Or { lhs; rhs }) with
                   | Rec_value expr -> expr
                   | _ -> Value (Literal (Bool false)))
                 | _ -> Value (Literal (Bool false)))
               (Value (Literal (Bool false))) args)
        | _ -> Rec_value e)
      | Map ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          Rec_value
            (Value
               (Literal
                  (Coll
                     ( ct,
                       CCList.map
                         (fun e_replace ->
                           evaluate_record_item
                             (replace_record_item_in_record_item body
                                (Rec_value (Value (LambdaVariable e_check)))
                                e_replace))
                         args ))))
        | _ -> Rec_value e)
      | Filter ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          Rec_value
            (Value
               (Literal
                  (Coll
                     ( ct,
                       CCList.filter
                         (fun e_replace ->
                           is_true
                             (evaluate_record_item
                                (replace_record_item_in_record_item body
                                   (Rec_value (Value (LambdaVariable e_check)))
                                   e_replace)))
                         args ))))
        | _ -> Rec_value e)
      | Find ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          (match
             CCList.find_opt
               (fun e_replace ->
                 is_true
                   (evaluate_record_item
                      (replace_record_item_in_record_item body
                         (Rec_value (Value (LambdaVariable e_check))) e_replace)))
               args
           with
          | None -> e_none
          | Some r -> r)
        | _ -> Rec_value e)
      | _ -> Rec_value e)
    | Rec_repeating_group { elements : record list; _ } ->
      (match hof_type with
      | For_all ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          evaluate_expr
            (CCList.fold_left
               (fun rhs e_replace ->
                 let repl =
                   evaluate_record_item
                     (replace_record_item_in_record_item body
                        (Rec_value (Value (LambdaVariable e_check))) e_replace)
                 in
                 match repl with
                 | Rec_value lhs ->
                   (match evaluate_expr (And { lhs; rhs }) with
                   | Rec_value expr -> expr
                   | _ -> Value (Literal (Bool false)))
                 | _ -> Value (Literal (Bool false)))
               (Value (Literal (Bool true)))
               (CCList.map (fun r -> Rec_record r) elements))
        | _ -> Rec_value e)
      | Exists ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          evaluate_expr
            (CCList.fold_left
               (fun rhs e_replace ->
                 let repl =
                   evaluate_record_item
                     (replace_record_item_in_record_item body
                        (Rec_value (Value (LambdaVariable e_check))) e_replace)
                 in
                 match repl with
                 | Rec_value lhs ->
                   (match evaluate_expr (Or { lhs; rhs }) with
                   | Rec_value expr -> expr
                   | _ -> Value (Literal (Bool false)))
                 | _ -> Value (Literal (Bool false)))
               (Value (Literal (Bool false)))
               (CCList.map (fun r -> Rec_record r) elements))
        | _ -> Rec_value e)
      | Map ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          Rec_value
            (Value
               (Literal
                  (Coll
                     ( List,
                       CCList.map
                         (fun e_replace ->
                           evaluate_record_item
                             (replace_record_item_in_record_item body
                                (Rec_value (Value (LambdaVariable e_check)))
                                e_replace))
                         (CCList.map (fun r -> Rec_record r) elements) ))))
        | _ -> Rec_value e)
      | Filter ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          Rec_value
            (Value
               (Literal
                  (Coll
                     ( List,
                       CCList.filter
                         (fun e_replace ->
                           is_true
                             (evaluate_record_item
                                (replace_record_item_in_record_item body
                                   (Rec_value (Value (LambdaVariable e_check)))
                                   e_replace)))
                         (CCList.map (fun r -> Rec_record r) elements) ))))
        | _ -> Rec_value e)
      | Find ->
        (match lambda_args with
        | [ LambdaVariable e_check ] ->
          (match
             CCList.find_opt
               (fun e_replace ->
                 is_true
                   (evaluate_record_item
                      (replace_record_item_in_record_item body
                         (Rec_value (Value (LambdaVariable e_check))) e_replace)))
               (CCList.map (fun r -> Rec_record r) elements)
           with
          | None -> e_none
          | Some r -> r)
        | _ -> Rec_value e)
      | _ -> Rec_value e)
    | _ ->
      let body = evaluate_record_item body in
      Rec_value
        (Value (Funcall { func = Hof { hof_type; lambda_args; body }; args })))
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
      | Value (Literal (Datetime dt1)), _, Value (Literal (Datetime dt2)) ->
        eval_add_datetime dt1 op dt2
      | _, '+', _ | _, '-', _ -> Rec_value (Add { lhs; op; rhs })
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
      | _, '*', _ | _, '/', _ -> Rec_value (Mul { lhs; op; rhs })
      | _, _, _ -> failwith "Unknown Mul operator")
    | _ -> Rec_value e)
  | In { el : expr; set : value } ->
    (match evaluate_expr (Value set) with
    | Rec_value (Value (Literal (Coll (_, es)))) ->
      let el = evaluate_expr el in
      if List.mem el (List.map evaluate_record_item es) then
        e_true
      else if is_ground el && List.for_all is_ground es then
        e_false
      else
        Rec_value e
    | _ -> Rec_value e)
  | _ -> Rec_value e

and normalise_to_is_set record_item =
  match record_item with
  | Rec_value expr ->
    Rec_value
      (walk
         (function
           | Eq { lhs; rhs = Rec_value (Value (Literal LiteralNone)) } ->
             Not
               (Value
                  (Funcall { func = Literal (String "IsSet"); args = [ lhs ] }))
           | Eq { lhs = Rec_value (Value (Literal LiteralNone)); rhs } ->
             Not
               (Value
                  (Funcall { func = Literal (String "IsSet"); args = [ rhs ] }))
           | Not (Eq { lhs; rhs = Rec_value (Value (Literal LiteralNone)) }) ->
             Value (Funcall { func = Literal (String "IsSet"); args = [ lhs ] })
           | Not (Eq { lhs = Rec_value (Value (Literal LiteralNone)); rhs }) ->
             Value (Funcall { func = Literal (String "IsSet"); args = [ rhs ] })
           | e -> e)
         expr)
  | Rec_record { name; elements } ->
    Rec_record { name; elements = String_map.map normalise_to_is_set elements }
  | Rec_repeating_group
      { name; message_template; num_in_group_field; elements : record list } ->
    Rec_repeating_group
      {
        name;
        message_template;
        num_in_group_field;
        elements =
          CCList.map
            (function
              | { name; elements } ->
                { name; elements = String_map.map normalise_to_is_set elements })
            elements;
      }

and fix_evaluate_record_item context record_item =
  let res = evaluate_record_item context record_item in
  if res = record_item then
    normalise_to_is_set record_item
  else
    fix_evaluate_record_item context res

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
          Value
            (Funcall
              {
                func = Literal (String "defaultIfNotSet");
                args = [ def_val; ri ];
              });
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
              (Funcall
                {
                  func = Literal (String "defaultIfNotSet");
                  args = [ def_val; ri ];
                }));
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
            replace_expr_in_expr expr
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
