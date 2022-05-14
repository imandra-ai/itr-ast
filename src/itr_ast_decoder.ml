module Itr_ast_pp (Datetime : Itr_ast.Datetime) = struct
  module I = Itr_ast.Itr_ast (Datetime)
  module D = Decoders_yojson.Basic.Decode

  let name_and_index_decoder : (string * int option) D.decoder =
    let open D in
    let* name = field "name" string in
    let* index = field "index" (nullable int) in
    succeed (name, index)


  let message_value_decoder : I.Message_value.t D.decoder =
    let open D in
    let* var_name = field "var_name" (nullable string) in
    let var = match var_name with None -> None | Some v -> Some v in
    let* field_path = field "field_path" (list name_and_index_decoder) in
    let mv : I.Message_value.t = { var; field_path } in
    succeed mv


  let datetime_decoder : I.datetime D.decoder =
    let open D in
    D.single_field (function
        | "UTCTimestamp" ->
            let+ u = Datetime.utctimestamp_decoder in
            I.UTCTimestamp u
        | "UTCTimeOnly" ->
            let+ u = Datetime.utctimeonly_decoder in
            I.UTCTimeOnly u
        | "UTCDateOnly" ->
            let+ u = Datetime.utcdateonly_decoder in
            I.UTCDateOnly u
        | "LocalMktDate" ->
            let+ u = Datetime.localmktdate_decoder in
            I.LocalMktDate u
        | "MonthYear" ->
            let+ u = Datetime.monthyear_decoder in
            I.MonthYear u
        | "Duration" ->
            let+ u = Datetime.duration_decoder in
            I.Duration u
        | s ->
            fail @@ "unrecognised datetime: " ^ s )


  let rec literal_decoder () : I.literal D.decoder =
    let open D in
    single_field (function
        | "Bool" ->
            let+ b = bool in
            I.Bool b
        | "Int" ->
            let+ i = string in
            I.Int (Z.of_string i)
        | "String" ->
            let+ s = string in
            if CCString.length s > 2
            then I.String (CCString.sub s 1 (CCString.length s - 2))
            else String ""
        | "Float" ->
            let+ q = string in
            I.Float (Q.of_string q)
        | "Coll" ->
            let+ c =
              list
                (one_of
                   [ ("record_item", record_item_decoder ())
                   ; ( "expr"
                     , let+ e = expr_decoder () in
                       I.Rec_value e )
                   ] )
            in
            I.Coll c
        | "MapColl" ->
            let* d = field "default" (record_item_decoder ()) in
            let+ c = field "elements" (list (record_item_pair_decoder ())) in
            I.MapColl (d, c)
        | "None" ->
            succeed I.LiteralNone
        | "Some" ->
            let+ e =
              one_of
                [ ("record_item", record_item_decoder ())
                ; ( "expr"
                  , let+ e = expr_decoder () in
                    I.Rec_value e )
                ]
            in
            I.LiteralSome e
        | "Datetime" ->
            let+ d = datetime_decoder in
            I.Datetime d
        | s ->
            fail @@ "unrecognised literal: " ^ s )


  and record_item_pair_decoder () : (I.record_item * I.record_item) D.decoder =
    let open D in
    let* key = field "key" (record_item_decoder ()) in
    let* value = field "value" (record_item_decoder ()) in
    succeed (key, value)


  and value_decoder () : I.value D.decoder =
    let open D in
    single_field (function
        | "Literal" ->
            let+ l = literal_decoder () in
            I.Literal l
        | "Variable" ->
            let+ v = string in
            I.Variable v
        | "Message_value" ->
            let+ mv = message_value_decoder in
            I.MessageValue mv
        | "ObjectProperty" ->
            let* obj =
              field
                "obj"
                (one_of
                   [ ("record_item", record_item_decoder ())
                   ; ( "expr"
                     , let+ e = expr_decoder () in
                       I.Rec_value e )
                   ] )
            in
            let* index = field "index" (nullable int) in
            let* prop = field "prop" string in
            succeed (I.ObjectProperty { obj; index; prop })
        | "Funcall" ->
            let* func = field "name" string in
            let* args =
              field
                "args"
                (list
                   (one_of
                      [ ("record_item", record_item_decoder ())
                      ; ( "expr"
                        , let+ e = expr_decoder () in
                          I.Rec_value e )
                      ] ) )
            in
            succeed (I.Funcall { func; args })
        | "CaseSplit" ->
            let* default_value =
              field "default_value" (record_item_decoder ())
            in
            let+ cases =
              field
                "cases"
                (list
                   (let* check = field "check" (record_item_decoder ()) in
                    let+ value = field "value" (record_item_decoder ()) in
                    (check, value) ) )
            in
            I.CaseSplit { default_value; cases }
        | "DataSetValue" ->
            let* name = field "name" string in
            let* field_name = field "field_name" string in
            let* default = field "default" (record_item_decoder ()) in
            let+ constraints =
              field "constraints" (list (record_item_decoder ()))
            in
            I.DataSetValue { name; field_name; default; constraints }
        | s ->
            fail @@ "unrecognised value:" ^ s )


  and expr_decoder () : I.expr D.decoder =
    let open D in
    single_field (function
        | "Value" ->
            let+ v = value_decoder () in
            I.Value v
        | "Not" ->
            let+ n = expr_decoder () in
            I.Not n
        | "Or" ->
            let* lhs = field "lhs" (expr_decoder ()) in
            let* rhs = field "rhs" (expr_decoder ()) in
            succeed (I.Or { lhs; rhs })
        | "And" ->
            let* lhs = field "lhs" (expr_decoder ()) in
            let* rhs = field "rhs" (expr_decoder ()) in
            succeed (I.And { lhs; rhs })
        | "Eq" ->
            let* lhs =
              one_of
                [ ("record_item", field "lhs" (record_item_decoder ()))
                ; ( "expr"
                  , field
                      "lhs"
                      (expr_decoder () >>= fun e -> succeed (I.Rec_value e)) )
                ]
            in
            let+ rhs =
              one_of
                [ ("record_item", field "rhs" (record_item_decoder ()))
                ; ( "expr"
                  , field
                      "rhs"
                      (expr_decoder () >>= fun e -> succeed (I.Rec_value e)) )
                ]
            in
            I.Eq { lhs; rhs }
        | "Cmp" ->
            let* lhs = field "lhs" (expr_decoder ()) in
            let* op = field "op" string in
            let* rhs = field "rhs" (expr_decoder ()) in
            succeed (I.Cmp { lhs; op; rhs })
        | "Add" ->
            let* lhs = field "lhs" (expr_decoder ()) in
            let* op = field "op" string in
            let op =
              if CCString.length op >= 1 then (CCString.sub op 0 1).[0] else '+'
            in
            let* rhs = field "rhs" (expr_decoder ()) in
            succeed (I.Add { lhs; op; rhs })
        | "Mul" ->
            let* lhs = field "lhs" (expr_decoder ()) in
            let* op = field "op" string in
            let op =
              if CCString.length op >= 1 then (CCString.sub op 0 1).[0] else '*'
            in
            let* rhs = field "rhs" (expr_decoder ()) in
            succeed (I.Mul { lhs; op; rhs })
        | "In" ->
            let* el = field "el" (expr_decoder ()) in
            let* set = field "set" (value_decoder ()) in
            succeed (I.In { el; set })
        | s ->
            fail @@ "unrecognised expr: " ^ s )


  and record_item_decoder () : I.record_item D.decoder =
    let open D in
    single_field (function
        | "Rec_value" ->
            let+ v = expr_decoder () in
            I.Rec_value v
        | "Rec_record" ->
            let+ is = record_decoder () in
            I.Rec_record is
        | "Rec_repeating_group" ->
            let* num_in_group_field = field "num_in_group_field" string in
            let* elements = field "elements" (list (record_decoder ())) in
            let* name = field "name" D.string in
            let* message_template =
              field "message_template" (nullable string)
            in
            succeed
              (I.Rec_repeating_group
                 { num_in_group_field; elements; name; message_template } )
        | s ->
            fail @@ "unrecognised record_item: " ^ s )


  and record_decoder () : I.record D.decoder =
    let open D in
    field
      "Record"
      (let* elements =
         field
           "elements"
           (list
              (let* name = field "name" string in
               let* record_item =
                 field "record_item" (record_item_decoder ())
               in
               succeed (name, record_item) ) )
       in
       let* name = field "name" string in
       succeed I.{ name; elements = String_map.of_list elements } )


  let expecting_decoder : I.expecting D.decoder =
    let open D in
    one_of
      [ ( "combined_expressions"
        , let+ exprs = field "exprs" (list (expr_decoder ())) in
          I.
            { relevant_exprs = exprs
            ; nullable_exprs = []
            ; qe_modified_exprs = []
            ; common_exprs = []
            } )
      ; ( "structured_expressions"
        , let* relevant_exprs =
            field "relevant_exprs" (list (expr_decoder ()))
          in
          let* common_exprs = field "common_exprs" (list (expr_decoder ())) in
          let* nullable_exprs =
            field
              "nullable_exprs"
              (one_of
                 [ ( "single_nullable"
                   , list
                       (let* field_paths =
                          field
                            "field_paths"
                            (list (list name_and_index_decoder))
                        in
                        let+ expr = field "expr" (expr_decoder ()) in
                        (expr, CCList.map (fun x -> [ x ]) field_paths) ) )
                 ; ( "set_nullable"
                   , list
                       (let* field_paths =
                          field
                            "field_paths"
                            (list (list (list name_and_index_decoder)))
                        in
                        let+ expr = field "expr" (expr_decoder ()) in
                        (expr, field_paths) ) )
                 ] )
          in
          let+ qe_modified_exprs =
            field "qe_modified_exprs" (list (expr_decoder ()))
          in
          I.{ relevant_exprs; nullable_exprs; qe_modified_exprs; common_exprs }
        )
      ]


  let field_decoder () : I.field D.decoder =
    let open D in
    let* name = field "name" string in
    let+ value = field "value" (record_item_decoder ()) in
    I.{ name; value }


  let instruction_decoder () : I.instruction D.decoder =
    let open D in
    single_field (function
        | "Action" ->
            let* name = field "name" string in
            let+ fields = field "fields" (list (field_decoder ())) in
            I.Action { name; fields }
        | "Message" ->
            let* name = field "name" string in
            let+ fields = field "fields" (list (field_decoder ())) in
            I.Message { name; fields }
        | "Set" ->
            let* prop = field "prop" string in
            let* value = field "value" (record_item_decoder ()) in
            succeed (I.Set { prop; value })
        | "Send" ->
            let* variable = field "variable" (nullable string) in
            let* tag = field "tag" string in
            let* withs = field "withs" (nullable (record_decoder ())) in
            succeed (I.Send { variable; tag; withs })
        | "Receive" ->
            let* variable = field "variable" (nullable string) in
            let* where = field "where" (expr_decoder ()) in
            let* expecting = field "expecting" (nullable expecting_decoder) in
            let+ example = field "example" (record_decoder ()) in
            I.Receive { variable; where; expecting; example }
        | "Prompt" ->
            let* prop = field "prop" string in
            let+ and_set = field "and_set" bool in
            I.Prompt { prop; and_set }
        | _ ->
            fail "unrecognised instruction" )
  (*
leaving here to put back for ipl-worker


let instructions_decoder :
    (instruction list * Model_messages.model_msg_opt_def list) decoder =
  let* instructions = field "instructions" (list (instruction_decoder ())) in
  let* msgs =
    field "msgs" (list Json_to_message.model_message_decoder_opt_def)
  in
  succeed (instructions, msgs)

*)
end
