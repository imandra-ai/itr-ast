module Itr_ast_pp (Datetime : Itr_ast.Datetime) = struct
  open Format

  open Itr_ast.Itr_ast (Datetime)

  let datetime_pp (ppf : formatter) : datetime -> unit = function
    | UTCTimestamp d ->
        fprintf ppf "%s" (Datetime.encode_UTCTimestamp d)
    | UTCTimeOnly d ->
        fprintf ppf "%s" (Datetime.encode_UTCTimeOnly d)
    | UTCDateOnly d ->
        fprintf ppf "%s" (Datetime.encode_UTCDateOnly d)
    | LocalMktDate d ->
        fprintf ppf "%s" (Datetime.encode_LocalMktDate d)
    | MonthYear d ->
        fprintf ppf "%s" (Datetime.encode_MonthYear d)
    | Duration d ->
        fprintf ppf "%s" (Datetime.encode_Duration d)


  let rec literal_pp (ppf : formatter) : literal -> unit = function
    | Bool true ->
        fprintf ppf "true"
    | Bool false ->
        fprintf ppf "false"
    | Int i ->
        fprintf ppf "%s" (Z.to_string i)
    | String s ->
        fprintf ppf "\"%s\"" (CCString.escaped s)
    | Float q ->
        fprintf ppf "%s" (Q.to_string q)
    | Coll l ->
        fprintf ppf "%a" CCFormat.(list ~sep:(return " ") record_item_pp) l
    | MapColl (d, l) ->
        fprintf
          ppf
          "default:%a@, %a"
          record_item_pp
          d
          CCFormat.(list ~sep:(return " ") record_item_pair_pp)
          l
    | LiteralNone ->
        fprintf ppf "None"
    | LiteralSome e ->
        fprintf ppf "Some %a" record_item_pp e
    | Datetime d ->
        fprintf ppf "%a" datetime_pp d


  and case_split_pair_pp (ppf : formatter) : record_item * record_item -> unit =
    function
    | check, value ->
        fprintf ppf "if (%a) : %a" record_item_pp check record_item_pp value


  and value_pp (ppf : formatter) : value -> unit = function
    | Literal l ->
        literal_pp ppf l
    | Variable v ->
        fprintf ppf "$%s" v
    | MessageValue mv ->
        fprintf ppf "%a" Message_value.pp mv
    | ObjectProperty op ->
        fprintf
          ppf
          "%a%a.%s"
          record_item_pp_parens
          op.obj
          opt_index_pp
          op.index
          op.prop
    | Funcall { func; args } ->
        fprintf
          ppf
          "%s(%a)"
          func
          CCFormat.(list ~sep:(return ",") record_item_pp)
          args
    | CaseSplit { default_value; cases } ->
        fprintf
          ppf
          "cases(%a@,default:%a)"
          CCFormat.(list ~sep:(return "@,") case_split_pair_pp)
          cases
          record_item_pp
          default_value
    | DataSetValue { name; field_name; default; constraints } ->
        fprintf
          ppf
          "dataset_value(%s,%s,%a) where [%a]"
          name
          field_name
          record_item_pp
          default
          CCFormat.(list ~sep:(return ",") record_item_pp)
          constraints


  and opt_index_pp (ppf : formatter) : int option -> unit = function
    | None ->
        ()
    | Some i ->
        fprintf ppf "[%i]" i


  and expr_pp (ppf : formatter) : expr -> unit = function
    | Value v ->
        fprintf ppf "%a" value_pp v
    | Not e ->
        fprintf ppf "!%a" expr_pp_parens e
    | Or { lhs; rhs } ->
        fprintf ppf "%a || %a" expr_pp_parens lhs expr_pp_parens rhs
    | And { lhs; rhs } ->
        fprintf ppf "%a && %a" expr_pp_parens lhs expr_pp_parens rhs
    | Eq { lhs; rhs } ->
        fprintf
          ppf
          "%a = %a"
          record_item_pp_parens
          lhs
          record_item_pp_parens
          rhs
    | Cmp { lhs; op; rhs } ->
        fprintf ppf "%a %s %a" expr_pp_parens lhs op expr_pp_parens rhs
    | Add { lhs; op; rhs } ->
        fprintf ppf "%a %c %a" expr_pp_parens lhs op expr_pp_parens rhs
    | Mul { lhs; op; rhs } ->
        fprintf ppf "%a %c %a" expr_pp_parens lhs op expr_pp_parens rhs
    | In { el; set } ->
        fprintf ppf "%a in %a" expr_pp_parens el value_pp set


  and record_item_pair_pp ppf (e1, e2) =
    fprintf ppf "%a -> %a" record_item_pp e1 record_item_pp e2


  and expr_pp_parens ppf e =
    match e with
    | Value _ | Not _ ->
        expr_pp ppf e
    | Or _ | And _ | Eq _ | Cmp _ | Add _ | Mul _ | In _ ->
        fprintf ppf "(%a)" expr_pp e


  and record_pp (ppf : formatter) (items : record) =
    let pp_item (ppf : formatter) ((name : string), (item : record_item)) =
      let open CCFormat in
      match item with
      | Rec_value _ ->
          fprintf ppf "%s = %a;" name record_item_pp item
      | Rec_record _ ->
          fprintf ppf "%s %a" name record_item_pp item
      | Rec_repeating_group _ ->
          record_item_pp ppf item
    in
    fprintf
      ppf
      "{@;<1 2>@[<v>%a@]@,}"
      CCFormat.(list ~sep:(return "@,") pp_item)
      (String_map.to_list items.elements |> CCList.rev)


  and record_item_pp (ppf : formatter) (item : record_item) =
    let open CCFormat in
    match item with
    | Rec_value value ->
        expr_pp ppf value
    | Rec_record items ->
        record_pp ppf items
    | Rec_repeating_group { num_in_group_field; elements; _ } ->
      ( match elements with
      | [] ->
          fprintf ppf "@[<v 2>%s@ [@[<v 1>@ {@,}@]@ ]@]" num_in_group_field
      | _ ->
          fprintf
            ppf
            "@[<v 2>%s@ [@[<v 1>@ %a@]@ ]@]"
            num_in_group_field
            (list ~sep:(return "@ ") record_pp)
            elements )


  and record_item_pp_parens (ppf : formatter) (item : record_item) =
    let open CCFormat in
    match item with
    | Rec_value value ->
        expr_pp_parens ppf value
    | Rec_record items ->
        record_pp ppf items
    | Rec_repeating_group { num_in_group_field; elements; _ } ->
      ( match elements with
      | [] ->
          fprintf ppf "@[<v 2>%s@ [@[<v 1>@ {@,}@]@ ]@]" num_in_group_field
      | _ ->
          fprintf
            ppf
            "@[<v 2>%s@ [@[<v 1>@ %a@]@ ]@]"
            num_in_group_field
            (list ~sep:(return "@ ") record_pp)
            elements )


  let rec instruction_pp (ppf : formatter) : instruction -> unit =
    let open CCFormat in
    let pp_var_eq_opt fmt = function
      | None ->
          ()
      | Some var ->
          fprintf fmt "%s = " var
    in
    function
    | Action _ ->
        ()
    | Message _ ->
        ()
    | Set { prop; value } ->
        fprintf ppf "set %s = %a" prop record_item_pp_parens value
    | Send { variable; tag; withs } ->
        fprintf
          ppf
          "@[<v>send %a(%s) %a@]"
          pp_var_eq_opt
          variable
          tag
          (CCFormat.some record_pp)
          withs
    | Prompt { prop; and_set } ->
        fprintf
          ppf
          "@[<v>prompt %s %s@]"
          (if and_set then "prompt" else "prompt_and_set")
          prop
    | Receive { variable; where; expecting; _ } ->
        let pp_space_opt fmt = function
          | None ->
              ()
          | Some x ->
              fprintf fmt " %s" x
        in
        let pp_expecting_opt pp fmt = function
          | None ->
              ()
          | Some x ->
              fprintf fmt "@ expecting %a" pp x
        in
        fprintf
          ppf
          "@[<v 2>receive%a where %a%a@]"
          pp_space_opt
          variable
          expr_pp
          where
          (pp_expecting_opt expecting_pp)
          expecting


  and expecting_pp fmt (expecting : expecting) =
    let open CCFormat in
    let es =
      expecting.relevant_exprs
      @ expecting.common_exprs
      @ expecting.qe_modified_exprs
      @ List.map fst expecting.nullable_exprs
    in
    fprintf fmt "{@,@[<v 2>  %a@]@,}" exprs_pp es


  and exprs_pp fmt (exprs : expr list) =
    let open CCFormat in
    fprintf fmt "%a" (list ~sep:(return "@,") expr_pp) exprs


  let instructions_pp = CCFormat.(vbox (list instruction_pp ~sep:(return "@,")))
end
