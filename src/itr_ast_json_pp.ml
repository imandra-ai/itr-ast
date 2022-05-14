module Itr_ast_json_pp (Datetime : Itr_ast.Datetime) = struct
  open Format

  open Itr_ast.Itr_ast (Datetime)

  open Yojson.Basic
  module JU = Yojson.Basic.Util

  let datetime_to_json : datetime -> t = function
    | UTCTimestamp d ->
        `Assoc [ ("UTCTimestamp", Datetime.utctimestamp_to_json d) ]
    | UTCTimeOnly d ->
        `Assoc [ ("UTCTimeOnly", Datetime.utctimeonly_to_json d) ]
    | UTCDateOnly d ->
        `Assoc [ ("UTCDateOnly", Datetime.utcdateonly_to_json d) ]
    | LocalMktDate d ->
        `Assoc [ ("LocalMktDate", Datetime.localmktdate_to_json d) ]
    | MonthYear d ->
        `Assoc [ ("MonthYear", Datetime.monthyear_to_json d) ]
    | Duration d ->
        `Assoc [ ("Duration", Datetime.duration_to_json d) ]


  let rec literal_to_json : literal -> t = function
    | Bool b ->
        `Assoc [ ("Bool", `Bool b) ]
    | Int i ->
        `Assoc [ ("Int", `String (Z.to_string i)) ]
    | String s ->
        `Assoc [ ("String", `String (sprintf "\"%s\"" s)) ]
    | Float q ->
        `Assoc [ ("Float", `String (Q.to_string q)) ]
    | Coll l ->
        `Assoc [ ("Coll", `List (List.map record_item_to_json l)) ]
    | MapColl (d, l) ->
        `Assoc
          [ ( "MapColl"
            , `Assoc
                [ ("default", record_item_to_json d)
                ; ("elements", `List (List.map record_item_pair_to_json l))
                ] )
          ]
    | LiteralNone ->
        `Assoc [ ("None", `String "None") ]
    | LiteralSome e ->
        `Assoc [ ("Some", record_item_to_json e) ]
    | Datetime d ->
        `Assoc [ ("Datetime", datetime_to_json d) ]


  and record_item_pair_to_json : record_item * record_item -> t = function
    | e1, e2 ->
        `Assoc
          [ ("key", record_item_to_json e1); ("value", record_item_to_json e2) ]


  and value_to_json : value -> t = function
    | Literal l ->
        `Assoc [ ("Literal", literal_to_json l) ]
    | Variable v ->
        `Assoc [ ("Variable", `String v) ]
    | MessageValue mv ->
        `Assoc [ ("Message_value", Message_value.to_json mv) ]
    | ObjectProperty op ->
        `Assoc
          [ ( "ObjectProperty"
            , `Assoc
                [ ("obj", record_item_to_json op.obj)
                ; ("index", opt_index_to_json op.index)
                ; ("prop", `String op.prop)
                ] )
          ]
    | Funcall { func; args } ->
        `Assoc
          [ ( "Funcall"
            , `Assoc
                [ ("name", `String func)
                ; ("args", `List (List.map record_item_to_json args))
                ] )
          ]
    | CaseSplit { default_value; cases } ->
        `Assoc
          [ ( "CaseSplit"
            , `Assoc
                [ ("default_value", record_item_to_json default_value)
                ; ( "cases"
                  , `List
                      (List.map
                         (fun (c, v) ->
                           `Assoc
                             [ ("check", record_item_to_json c)
                             ; ("value", record_item_to_json v)
                             ] )
                         cases ) )
                ] )
          ]
    | DataSetValue { name; field_name; default; constraints } ->
        `Assoc
          [ ( "DataSetValue"
            , `Assoc
                [ ("name", `String name)
                ; ("field_name", `String field_name)
                ; ("default", record_item_to_json default)
                ; ( "constraints"
                  , `List (List.map record_item_to_json constraints) )
                ] )
          ]


  and opt_index_to_json : int option -> t = function
    | None ->
        `Null
    | Some i ->
        `Int i


  and expr_to_json : expr -> t = function
    | Value v ->
        `Assoc [ ("Value", value_to_json v) ]
    | Not e ->
        `Assoc [ ("Not", expr_to_json e) ]
    | Or { lhs; rhs } ->
        `Assoc
          [ ( "Or"
            , `Assoc [ ("lhs", expr_to_json lhs); ("rhs", expr_to_json rhs) ] )
          ]
    | And { lhs; rhs } ->
        `Assoc
          [ ( "And"
            , `Assoc [ ("lhs", expr_to_json lhs); ("rhs", expr_to_json rhs) ] )
          ]
    | Eq { lhs; rhs } ->
        `Assoc
          [ ( "Eq"
            , `Assoc
                [ ("lhs", record_item_to_json lhs)
                ; ("rhs", record_item_to_json rhs)
                ] )
          ]
    | Cmp { lhs; op; rhs } ->
        `Assoc
          [ ( "Cmp"
            , `Assoc
                [ ("lhs", expr_to_json lhs)
                ; ("op", `String op)
                ; ("rhs", expr_to_json rhs)
                ] )
          ]
    | Add { lhs; op; rhs } ->
        `Assoc
          [ ( "Add"
            , `Assoc
                [ ("lhs", expr_to_json lhs)
                ; ("op", `String (Char.escaped op))
                ; ("rhs", expr_to_json rhs)
                ] )
          ]
    | Mul { lhs; op; rhs } ->
        `Assoc
          [ ( "Mul"
            , `Assoc
                [ ("lhs", expr_to_json lhs)
                ; ("op", `String (Char.escaped op))
                ; ("rhs", expr_to_json rhs)
                ] )
          ]
    | In { el; set } ->
        `Assoc
          [ ( "In"
            , `Assoc [ ("el", expr_to_json el); ("set", value_to_json set) ] )
          ]


  and record_to_json (record : record) =
    let item_to_json ((name : string), (item : record_item)) =
      `Assoc
        [ ("name", `String name); ("record_item", record_item_to_json item) ]
    in
    `Assoc
      [ ( "Record"
        , `Assoc
            [ ("name", `String record.name)
            ; ( "elements"
              , `List
                  (List.map
                     item_to_json
                     (String_map.to_list record.elements |> CCList.rev) ) )
            ] )
      ]


  and record_item_to_json (item : record_item) =
    match item with
    | Rec_value value ->
        `Assoc [ ("Rec_value", expr_to_json value) ]
    | Rec_record items ->
        `Assoc [ ("Rec_record", record_to_json items) ]
    | Rec_repeating_group
        { num_in_group_field; elements; name; message_template } ->
        `Assoc
          [ ( "Rec_repeating_group"
            , `Assoc
                [ ("num_in_group_field", `String num_in_group_field)
                ; ("elements", `List (List.map record_to_json elements))
                ; ("name", `String name)
                ; ( "message_template"
                  , match message_template with
                    | None ->
                        `Null
                    | Some message_template ->
                        `String message_template )
                ] )
          ]


  let field_to_json (f : field) =
    `Assoc [ ("name", `String f.name); ("value", record_item_to_json f.value) ]


  let rec instruction_to_json : instruction -> t = function
    | Action { name; fields } ->
        `Assoc
          [ ( "Action"
            , `Assoc
                [ ("name", `String name)
                ; ("fields", `List (List.map field_to_json fields))
                ] )
          ]
    | Message { name; fields } ->
        `Assoc
          [ ( "Message"
            , `Assoc
                [ ("name", `String name)
                ; ("fields", `List (List.map field_to_json fields))
                ] )
          ]
    | Set { prop; value } ->
        `Assoc
          [ ( "Set"
            , `Assoc
                [ ("prop", `String prop); ("value", record_item_to_json value) ]
            )
          ]
    | Prompt { prop; and_set } ->
        `Assoc
          [ ( "Prompt"
            , `Assoc [ ("prop", `String prop); ("and_set", `Bool and_set) ] )
          ]
    | Send { variable; tag; withs } ->
        `Assoc
          [ ( "Send"
            , `Assoc
                [ ( "variable"
                  , match variable with
                    | None ->
                        `Null
                    | Some variable ->
                        `String variable )
                ; ("tag", `String tag)
                ; ( "withs"
                  , match withs with
                    | None ->
                        `Null
                    | Some withs ->
                        record_to_json withs )
                ] )
          ]
    | Receive { variable; where; expecting; example } ->
        `Assoc
          [ ( "Receive"
            , `Assoc
                [ ( "variable"
                  , match variable with
                    | None ->
                        `Null
                    | Some variable ->
                        `String variable )
                ; ("where", expr_to_json where)
                ; ( "expecting"
                  , match expecting with
                    | None ->
                        `Null
                    | Some e ->
                        expecting_to_json e )
                ; ("example", record_to_json example)
                ] )
          ]


  and expecting_to_json (expecting : expecting) =
    `Assoc
      [ ( "relevant_exprs"
        , `List (List.map expr_to_json expecting.relevant_exprs) )
      ; ("common_exprs", `List (List.map expr_to_json expecting.common_exprs))
      ; ( "qe_modified_exprs"
        , `List (List.map expr_to_json expecting.qe_modified_exprs) )
      ; ( "nullable_exprs"
        , `List
            (List.map
               (fun (expr, field_paths_list) ->
                 `Assoc
                   [ ("expr", expr_to_json expr)
                   ; ( "field_paths"
                     , `List
                         (List.map
                            (fun field_paths ->
                              `List
                                (List.map
                                   (fun x ->
                                     `List
                                       (List.map
                                          (fun (x, o) ->
                                            `Assoc
                                              [ ("name", `String x)
                                              ; ( "index"
                                                , match o with
                                                  | None ->
                                                      `Null
                                                  | Some x ->
                                                      `Int x )
                                              ] )
                                          x ) )
                                   field_paths ) )
                            field_paths_list ) )
                   ] )
               expecting.nullable_exprs ) )
      ]

  (*
leaving this in comment for when reintroduced back to ipl-worker

let instructions_to_json is msgs_of_step =
  `Assoc
    [ ( "msgs"
      , `List
          (List.map Model_messages_json.json_of_model_msg_opt_def msgs_of_step)
      )
    ; ("instructions", `List (List.map instruction_to_json is))
    ]
*)
end
