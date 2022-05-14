module type Datetime = sig
  open Decoders_yojson.Basic.Decode
  open Yojson.Basic

  type fix_utctimestamp

  type fix_utctimeonly

  type fix_utcdateonly

  type fix_localmktdate

  type fix_monthyear

  type fix_duration

  val encode_UTCTimestamp : fix_utctimestamp -> string

  val encode_UTCTimeOnly : fix_utctimeonly -> string

  val encode_UTCDateOnly : fix_utcdateonly -> string

  val encode_LocalMktDate : fix_localmktdate -> string

  val encode_MonthYear : fix_monthyear -> string

  val encode_Duration : fix_duration -> string

  val utctimestamp_decoder : fix_utctimestamp decoder

  val utctimeonly_decoder : fix_utctimeonly decoder

  val utcdateonly_decoder : fix_utcdateonly decoder

  val localmktdate_decoder : fix_localmktdate decoder

  val monthyear_decoder : fix_monthyear decoder

  val duration_decoder : fix_duration decoder

  val utctimestamp_to_json : fix_utctimestamp -> Yojson.Basic.t

  val utctimeonly_to_json : fix_utctimeonly -> Yojson.Basic.t

  val utcdateonly_to_json : fix_utcdateonly -> Yojson.Basic.t

  val localmktdate_to_json : fix_localmktdate -> Yojson.Basic.t

  val monthyear_to_json : fix_monthyear -> Yojson.Basic.t

  val duration_to_json : fix_duration -> Yojson.Basic.t

  val utctimestamp_LessThan : fix_utctimestamp -> fix_utctimestamp -> bool

  val utctimeonly_LessThan : fix_utctimeonly -> fix_utctimeonly -> bool

  val utcdateonly_LessThan : fix_utcdateonly -> fix_utcdateonly -> bool

  val localmktdate_LessThan : fix_localmktdate -> fix_localmktdate -> bool

  val monthyear_LessThan : fix_monthyear -> fix_monthyear -> bool

  val utctimestamp_LessThanEqual : fix_utctimestamp -> fix_utctimestamp -> bool

  val utctimeonly_LessThanEqual : fix_utctimeonly -> fix_utctimeonly -> bool

  val utcdateonly_LessThanEqual : fix_utcdateonly -> fix_utcdateonly -> bool

  val localmktdate_LessThanEqual : fix_localmktdate -> fix_localmktdate -> bool

  val monthyear_LessThanEqual : fix_monthyear -> fix_monthyear -> bool

  val utctimestamp_GreaterThan : fix_utctimestamp -> fix_utctimestamp -> bool

  val utctimeonly_GreaterThan : fix_utctimeonly -> fix_utctimeonly -> bool

  val utcdateonly_GreaterThan : fix_utcdateonly -> fix_utcdateonly -> bool

  val localmktdate_GreaterThan : fix_localmktdate -> fix_localmktdate -> bool

  val monthyear_GreaterThan : fix_monthyear -> fix_monthyear -> bool

  val utctimestamp_GreaterThanEqual :
    fix_utctimestamp -> fix_utctimestamp -> bool

  val utctimeonly_GreaterThanEqual : fix_utctimeonly -> fix_utctimeonly -> bool

  val utcdateonly_GreaterThanEqual : fix_utcdateonly -> fix_utcdateonly -> bool

  val localmktdate_GreaterThanEqual :
    fix_localmktdate -> fix_localmktdate -> bool

  val monthyear_GreaterThanEqual : fix_monthyear -> fix_monthyear -> bool

  val convert_utctimestamp_utcdateonly : fix_utctimestamp -> fix_utcdateonly

  val convert_utctimestamp_localmktdate : fix_utctimestamp -> fix_localmktdate

  val get_current_utctimestamp : unit -> fix_utctimestamp
end

module Itr_ast (Datetime : Datetime) = struct
  module Map_extra (M : CCMap.S) = struct
    include M

    let merge_keep_left m1 m2 =
      merge_safe m1 m2 ~f:(fun _ -> function
        | `Left a -> Some a | `Right b -> Some b | `Both (a, _) -> Some a )


    let merge_all_keep_left = function
      | [] ->
          empty
      | m :: ms ->
          CCList.fold_left merge_keep_left m ms
  end

  module String_map = Map_extra (CCMap.Make (CCString))

  module Field_path_set = CCSet.Make (struct
    type t = (string * int option) list

    let compare = compare
  end)

  let pp_string fmt s = CCFormat.(fprintf fmt "%S" s)

  module Message_value = struct
    type t =
      { var : string option
      ; field_path : (string * int option) list
      }

    let mk ~var field_path = { var; field_path }

    let to_json t : Yojson.Basic.t =
      let pre : Yojson.Basic.t =
        match t.var with None -> `Null | Some v -> `String v
      in
      let post : string * Yojson.Basic.t =
        ( "field_path"
        , `List
            (List.map
               (fun (x, o) ->
                 `Assoc
                   [ ("name", `String x)
                   ; ("index", match o with None -> `Null | Some x -> `Int x)
                   ] )
               t.field_path ) )
      in
      let ent = [ ("var_name", pre); post ] in
      `Assoc ent


    let pp fmt t =
      let open CCFormat in
      let index_pp fmt = function
        | s, None ->
            fprintf fmt "%s" s
        | s, Some index ->
            fprintf fmt "%s[%i]" s index
      in
      let pp_var_opt fmt = function
        | None ->
            ()
        | Some v ->
            fprintf fmt "%s." v
      in
      fprintf
        fmt
        "%a%a"
        pp_var_opt
        t.var
        (list ~sep:(return ".") index_pp)
        t.field_path
  end

  type datetime =
    | UTCTimestamp of Datetime.fix_utctimestamp
    | UTCTimeOnly of Datetime.fix_utctimeonly
    | UTCDateOnly of Datetime.fix_utcdateonly
    | LocalMktDate of Datetime.fix_localmktdate
    | MonthYear of Datetime.fix_monthyear
    | Duration of Datetime.fix_duration

  type literal =
    | Bool of bool
    | Int of Z.t
    | String of string
    | Float of Q.t
    | Coll of record_item list
    | MapColl of record_item * (record_item * record_item) list
    | LiteralNone
    | LiteralSome of record_item
    | Datetime of datetime

  and value =
    | Literal of literal
    | Variable of string
    | MessageValue of Message_value.t
    | ObjectProperty of
        { obj : record_item
        ; index : int option
        ; prop : string
        }
    | Funcall of
        { func : string
        ; args : record_item list
        }
    | CaseSplit of
        { default_value : record_item
        ; cases : (record_item * record_item) list
        }
    | DataSetValue of
        { name : string
        ; field_name : string
        ; default : record_item
        ; constraints : record_item list
        }

  and expr =
    | Value of value
    | Not of expr
    | In of
        { el : expr
        ; set : value
        }
    | Or of
        { lhs : expr
        ; rhs : expr
        }
    | And of
        { lhs : expr
        ; rhs : expr
        }
    | Eq of
        { lhs : record_item
        ; rhs : record_item
        }
    | Cmp of
        { lhs : expr
        ; op : string
        ; rhs : expr
        }
    | Add of
        { lhs : expr
        ; op : char
        ; rhs : expr
        }
    | Mul of
        { lhs : expr
        ; op : char
        ; rhs : expr
        }

  and record =
    { name : string
    ; elements : record_item String_map.t
    }

  and record_item =
    | Rec_value of expr
    | Rec_record of record
    | Rec_repeating_group of
        { name : string
        ; message_template : string option
        ; num_in_group_field : string
        ; elements : record list
        }

  let rec expr_eq e1 e2 =
    match (e1, e2) with
    | Value v1, Value v2 ->
        value_eq v1 v2
    | Not e1, Not e2 ->
        expr_eq e1 e2
    | Or { lhs = lhs1; rhs = rhs1 }, Or { lhs = lhs2; rhs = rhs2 }
    | And { lhs = lhs1; rhs = rhs1 }, And { lhs = lhs2; rhs = rhs2 } ->
        expr_eq lhs1 lhs2 && expr_eq rhs1 rhs2
    | Eq { lhs = lhs1; rhs = rhs1 }, Eq { lhs = lhs2; rhs = rhs2 } ->
        record_item_eq lhs1 lhs2 && record_item_eq rhs1 rhs2
    | ( Cmp { lhs = lhs1; rhs = rhs1; op = op1 }
      , Cmp { lhs = lhs2; rhs = rhs2; op = op2 } ) ->
        op1 = op2 && expr_eq lhs1 lhs2 && expr_eq rhs1 rhs2
    | ( Add { lhs = lhs1; rhs = rhs1; op = op1 }
      , Add { lhs = lhs2; rhs = rhs2; op = op2 } )
    | ( Mul { lhs = lhs1; rhs = rhs1; op = op1 }
      , Mul { lhs = lhs2; rhs = rhs2; op = op2 } ) ->
        op1 = op2 && expr_eq lhs1 lhs2 && expr_eq rhs1 rhs2
    | _ ->
        false


  and value_eq v1 v2 =
    match (v1, v2) with
    | ( Funcall { func = func1; args = args1 }
      , Funcall { func = func2; args = args2 } ) ->
        func1 = func2 && CCList.equal record_item_eq args1 args2
    | _ ->
        v1 = v2


  and record_item_eq i1 i2 =
    match (i1, i2) with
    | Rec_value e1, Rec_value e2 ->
        expr_eq e1 e2
    | Rec_record r1, Rec_record r2 ->
        record_eq r1 r2
    | ( Rec_repeating_group
          { name = nm1; num_in_group_field = n1; elements = elements1; _ }
      , Rec_repeating_group
          { name = nm2; num_in_group_field = n2; elements = elements2; _ } ) ->
        nm1 = nm2 && n1 = n2 && CCList.equal record_eq elements1 elements2
    | _ ->
        false


  and record_eq r1 r2 =
    r1.name = r2.name && String_map.equal record_item_eq r1.elements r2.elements


  let map_value ~map_record_item = function
    | ObjectProperty { obj; index; prop } ->
        ObjectProperty { obj = map_record_item obj; index; prop }
    | Funcall { func; args } ->
        Funcall { func; args = List.map map_record_item args }
    | Literal (Coll l) ->
        Literal (Coll (List.map map_record_item l))
    | Literal (MapColl (d, l)) ->
        Literal
          (MapColl
             ( map_record_item d
             , List.map (fun (l, r) -> (map_record_item l, map_record_item r)) l
             ) )
    | Literal (LiteralSome ri) ->
        Literal (LiteralSome (map_record_item ri))
    | v ->
        v


  let map_expr ~map_expr:f ~map_value ~map_record_item = function
    | Value v ->
        Value (map_value v)
    | Not e ->
        Not (f e)
    | Or { lhs; rhs } ->
        Or { lhs = f lhs; rhs = f rhs }
    | And { lhs; rhs } ->
        And { lhs = f lhs; rhs = f rhs }
    | Eq { lhs; rhs } ->
        Eq { lhs = map_record_item lhs; rhs = map_record_item rhs }
    | Cmp { lhs; op; rhs } ->
        Cmp { lhs = f lhs; op; rhs = f rhs }
    | Add { lhs; op; rhs } ->
        Add { lhs = f lhs; op; rhs = f rhs }
    | Mul { lhs; op; rhs } ->
        Mul { lhs = f lhs; op; rhs = f rhs }
    | In { el; set } ->
        In { el = f el; set = map_value set }


  let map_record_item ~map_expr ~map_record = function
    | Rec_value expr ->
        Rec_value (map_expr expr)
    | Rec_record record ->
        Rec_record (map_record record)
    | Rec_repeating_group
        { name : string
        ; message_template : string option
        ; num_in_group_field
        ; elements
        } ->
        Rec_repeating_group
          { name
          ; message_template
          ; num_in_group_field
          ; elements = CCList.map map_record elements
          }


  let map_record ~map_record_item { name; elements } =
    { name; elements = String_map.map map_record_item elements }


  let map f =
    let rec mri item =
      map_record_item
        ~map_expr:f
        ~map_record:(map_record ~map_record_item:mri)
        item
    in
    map_expr
      ~map_expr:f
      ~map_value:(map_value ~map_record_item:mri)
      ~map_record_item:mri


  let rec walk f e =
    let e = f e in
    map (walk f) e


  exception Iterated

  let iterated () = raise Iterated

  let iter (f : expr -> unit) (e : expr) =
    try
      ignore
        (walk
           (fun e ->
             f e ;
             e )
           e )
    with
    | Iterated ->
        ()


  let exists (f : expr -> bool) (e : expr) =
    let found = ref false in
    iter
      (fun e ->
        if f e
        then (
          found := true ;
          iterated () ) )
      e ;
    !found


  let rec fold f init e =
    let init = f init e in
    match e with
    | Value _ ->
        init
    | Not e1 ->
        fold f init e1
    | Or { lhs; rhs }
    | And { lhs; rhs }
    | Cmp { lhs; rhs; _ }
    | Add { lhs; rhs; _ }
    | Mul { lhs; rhs; _ } ->
        let init = fold f init lhs in
        let init = fold f init rhs in
        init
    | Eq { lhs = Rec_value lhs; rhs = Rec_value rhs } ->
        let lhs = Rec_value (fold f init lhs) in
        let rhs = Rec_value (fold f init rhs) in
        Eq { lhs; rhs }
    | In { el; _ } ->
        let init = fold f init el in
        init
    | _ ->
        init


  let rec get_vars_record_item ri acc =
    match ri with
    | Rec_value e ->
        get_vars_expr e acc
    | Rec_record r ->
        CCList.fold_left
          (fun inner_acc (_, inner_el) ->
            get_vars_record_item inner_el inner_acc )
          acc
          (String_map.to_list r.elements)
    | Rec_repeating_group r ->
        CCList.fold_left
          (fun inner_acc inner_el ->
            CCList.fold_left
              (fun r_acc (_, r_el) -> get_vars_record_item r_el r_acc)
              inner_acc
              (String_map.to_list inner_el.elements) )
          acc
          r.elements


  and get_vars_expr e acc =
    match e with
    | Value (ObjectProperty { obj : record_item; _ }) ->
        get_vars_record_item obj acc
    | Value (Funcall { args : record_item list; _ }) ->
        CCList.fold_left
          (fun inner_acc inner_el -> get_vars_record_item inner_el inner_acc)
          acc
          args
    | Value _ ->
        e :: acc
    | Not e1 ->
        get_vars_expr e1 acc
    | Or { lhs; rhs }
    | And { lhs; rhs }
    | Cmp { lhs; rhs; _ }
    | Add { lhs; rhs; _ }
    | Mul { lhs; rhs; _ } ->
        let vars_l_acc = get_vars_expr lhs acc in
        let vars_r_acc = get_vars_expr rhs vars_l_acc in
        vars_r_acc
    | Eq { lhs = Rec_value lhs; rhs = Rec_value rhs } ->
        let vars_l_acc = get_vars_expr lhs acc in
        let vars_r_acc = get_vars_expr rhs vars_l_acc in
        vars_r_acc
    | In { el; _ } ->
        get_vars_expr el acc
    | _ ->
        acc


  let get_vars e = get_vars_expr e []

  let is_funcall = function Value (Funcall _) -> true | _ -> false

  let bool b = Value (Literal (Bool b))

  let string s = Value (Literal (String s))

  let int i = Value (Literal (Int i))

  let var v = Value (Variable v)

  let object_property obj ?index prop =
    Value (ObjectProperty { obj; index; prop })


  let message_value ?var field_path =
    Value
      (MessageValue
         { var; field_path = field_path |> CCList.map (fun f -> (f, None)) } )


  let funcall func args = Value (Funcall { func; args })

  let and_ lhs rhs = And { lhs; rhs }

  let or_ lhs rhs = Or { lhs; rhs }

  let gt lhs rhs = Cmp { lhs; op = ">"; rhs }

  let add lhs rhs = Add { lhs; rhs; op = '+' }

  let sub lhs rhs = Add { lhs; rhs; op = '-' }

  let is_set e = funcall "IsSet" [ e ]

  let eq lhs rhs = Eq { lhs; rhs }

  let eq_opt lhs rhs =
    or_ (and_ (Not (is_set lhs)) (Not (is_set rhs))) (eq lhs rhs)


  let add_eval lhs rhs =
    match (lhs, rhs) with
    | Value (Literal (String lhs)), Value (Literal (String rhs)) ->
        Value (Literal (String (lhs ^ rhs)))
    | _ ->
        add lhs rhs


  let concat ?sep = function
    | [] ->
        string ""
    | e :: es ->
        let add e1 e2 =
          match sep with
          | None ->
              add_eval e1 e2
          | Some sep ->
              add_eval e1 (add_eval sep e2)
        in
        CCList.fold_left add e es


  let simplify e =
    let rec simplify_rec e acc_string acc_list =
      match e with
      | [] ->
          acc_list
      | Value (Literal (String s)) :: t ->
          simplify_rec t (acc_string ^ s) acc_list
      | h :: t ->
          simplify_rec
            t
            ""
            ( acc_list
            @ (match acc_string with "" -> [] | _ -> [ string acc_string ])
            @ [ h ] )
    in
    concat (simplify_rec e "" [])


  let all = function
    | [] ->
        bool true
    | e :: es ->
        CCList.fold_left
          (fun e c ->
            match c with Value (Literal (Bool true)) -> e | _ -> and_ e c )
          e
          es


  let convert_timestamp_localmktdate date =
    funcall "timestamp_to_localmktdate" [ date ]


  let format_utctimestamp date =
    funcall "FormatDate" [ date; Rec_value (string "yyyyMMdd-HH:mm:ss.SSS") ]


  let format_localmktdate date =
    funcall "FormatDate" [ date; Rec_value (string "yyyyMMdd") ]


  let convert_timestamp_dateonly date = funcall "timestamp_to_dateonly" [ date ]

  let empty_record : record = { name = ""; elements = String_map.empty }

  let rec map_value_in_record (path : string list) f (record : record) =
    match path with
    | [] ->
        record
    | [ field ] ->
        { name = record.name
        ; elements = record.elements |> String_map.update field f
        }
    | field :: path ->
        { name = record.name
        ; elements =
            record.elements
            |> String_map.update field (fun entry ->
                   let record =
                     match entry with
                     | Some (Rec_record record) ->
                         record
                     | _ ->
                         empty_record
                   in
                   Some (Rec_record (map_value_in_record path f record)) )
        }


  (** Given a record, return a list of paths to expressions that reference all the
    fields in the record.

    E.g. the record

        Parties [
          { PartyID = "123" }
          { PartyID = "456" }
        ]

    Returns

       [ (["Parties[0]"; "PartyID"], "123")
       ; (["Parties[1]"; "PartyID"], "456")
       ]
*)
  let rec flatten_record ?qualify_records (r : record) :
      (string list * expr) list =
    r.elements
    |> String_map.to_list
    |> CCList.flat_map (fun (field, (item : record_item)) ->
           flatten_record_item ?qualify_records field item )


  and flatten_record_item ?(qualify_records = true) field (i : record_item) =
    match i with
    | Rec_value expr ->
        [ ([ field ], expr) ]
    | Rec_record record ->
        flatten_record ~qualify_records record
        |> CCList.map (fun (path, expr) ->
               if qualify_records then (field :: path, expr) else (path, expr) )
    | Rec_repeating_group { num_in_group_field; elements; _ } ->
        elements
        |> CCList.mapi (fun i record ->
               flatten_record ~qualify_records record
               |> CCList.map (fun (path, expr) ->
                      ( field
                        :: Printf.sprintf "%s[%i]" num_in_group_field i
                        :: path
                      , expr ) ) )
        |> CCList.concat


  type expecting =
    { relevant_exprs : expr list
    ; nullable_exprs : (expr * (string * int option) list list list) list
    ; qe_modified_exprs : expr list
    ; common_exprs : expr list
    }

  type field =
    { name : string
    ; value : record_item
    }

  type instruction =
    | Action of
        { name : string
        ; fields : field list
        }
    | Message of
        { name : string
        ; fields : field list
        }
    | Prompt of
        { prop : string
        ; and_set : bool
        }
    | Set of
        { prop : string
        ; value : record_item
        }
    | Send of
        { variable : string option
        ; tag : string
        ; withs : record option
        }
    | Receive of
        { variable : string option
        ; where : expr
        ; expecting : expecting option
        ; example : record
        }

  module Instructions_set = CCSet.Make (struct
    type t = instruction list

    let compare = compare
  end)

  module Value = struct
    type t = value

    let rec exists f v =
      f v
      ||
      match v with
      | Literal _ | Variable _ | MessageValue _ ->
          false
      | ObjectProperty { obj = e; index = _; prop = _ } ->
          exists_record_item f e
      | Funcall { func = _; args = es } ->
          CCList.exists (exists_record_item f) es
      | CaseSplit { default_value; cases } ->
          exists_record_item f default_value
          || CCList.exists
               (fun (check, value) ->
                 exists_record_item f check || exists_record_item f value )
               cases
      | DataSetValue { default; constraints; _ } ->
          exists_record_item f default
          || List.exists (exists_record_item f) constraints


    and exists_expr f = function
      | Value v ->
          exists f v
      | Not e ->
          exists_expr f e
      | Or { lhs; rhs }
      | And { lhs; rhs }
      | Cmp { lhs; rhs; op = _ }
      | Add { lhs; rhs; op = _ }
      | Mul { lhs; rhs; op = _ } ->
          exists_expr f lhs || exists_expr f rhs
      | In { el; set } ->
          exists_expr f el || exists f set
      | Eq { lhs; rhs } ->
          exists_record_item f lhs || exists_record_item f rhs


    and exists_record_item f = function
      | Rec_value e ->
          exists_expr f e
      | Rec_record record ->
          exists_record f record
      | Rec_repeating_group { num_in_group_field = _; elements; _ } ->
          CCList.exists (exists_record f) elements


    and exists_record f r =
      String_map.exists
        (fun _ record_item -> exists_record_item f record_item)
        r.elements


    let exists_instruction f = function
      | Set { prop = _; value = e } ->
          exists_record_item f e
      | Action { fields; _ } ->
          CCList.exists
            (exists_record_item f)
            (List.map (fun x -> x.value) fields)
      | Message { fields; _ } ->
          CCList.exists
            (exists_record_item f)
            (List.map (fun x -> x.value) fields)
      | Send { variable = _; withs = record; tag = _ } ->
          CCOption.exists (exists_record f) record
      | Receive { variable = _; where; expecting; _ } ->
          exists_expr f where
          || CCOption.exists
               (fun (e : expecting) ->
                 CCList.exists
                   (exists_expr f)
                   ( e.relevant_exprs
                   @ e.common_exprs
                   @ List.map fst e.nullable_exprs ) )
               expecting
      | Prompt _ ->
          false
  end

  let is_local_message_value = function
    | Value (MessageValue { var = None; _ }) ->
        true
    | _ ->
        false


  let set prop value = Set { prop; value }

  let not' e = Not e

  let send ?variable ~tag ?values () = Send { variable; withs = values; tag }

  let expecting relevant_exprs common_exprs qe_modified_exprs nullable_exprs =
    { relevant_exprs; common_exprs; qe_modified_exprs; nullable_exprs }


  let receive ?variable ~where ?expecting ~example () =
    Receive { variable; where; expecting; example }
end
