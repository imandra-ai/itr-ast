module I = Itr_ast
module D = Decoders_yojson.Basic.Decode

let version : int option ref = ref None

let name_and_index_decoder : (string * Z.t option) D.decoder =
  let open D in
  let* name = field "name" string in
  let* index = field "index" (nullable string) in
  succeed (name, CCOption.map Z.of_string index)

let message_value_decoder : I.Message_value.t D.decoder =
  let open D in
  let* var_name = field "var_name" (nullable string) in
  let var =
    match var_name with
    | None -> None
    | Some v -> Some v
  in
  let* field_path = field "field_path" (list name_and_index_decoder) in
  let mv : I.Message_value.t = { var; field_path } in
  succeed mv

let datetime_decoder : I.datetime D.decoder =
  let open D in
  D.single_field (function
    | "UTCTimestamp" ->
      let+ u = Datetime_json.utctimestamp_micro_decoder in
      I.UTCTimestamp u
    | "UTCTimeOnly" ->
      let+ u = Datetime_json.utctimeonly_micro_decoder in
      I.UTCTimeOnly u
    | "UTCDateOnly" ->
      let+ u = Datetime_json.utcdateonly_decoder in
      I.UTCDateOnly u
    | "LocalMktDate" ->
      let+ u = Datetime_json.localmktdate_decoder in
      I.LocalMktDate u
    | "MonthYear" ->
      let+ u = Datetime_json.monthyear_decoder in
      I.MonthYear u
    | "Duration" ->
      let+ u = Datetime_json.duration_decoder in
      I.Duration u
    | s -> fail @@ "unrecognised datetime: " ^ s)

let hof_type_decoder : I.hof_type D.decoder =
  let open D in
  D.single_field (function
    | "For_all" -> succeed I.For_all
    | "Exists" -> succeed I.Exists
    | "Map" -> succeed I.Map
    | "Filter" -> succeed I.Filter
    | "Find" -> succeed I.Find
    | s -> fail @@ "unrecognised hof_type: " ^ s)

let coll_type_decoder : I.coll_type D.decoder =
  let open D in
  D.single_field (function
    | "Tuple" -> succeed I.Tuple
    | "Set" -> succeed (I.Set : I.coll_type)
    | "List" -> succeed I.List
    | s -> fail @@ "unrecognised coll_type: " ^ s)

let rec literal_decoder () : I.literal D.decoder =
  let open D in
  single_field (function
    | "Bool" ->
      let+ b = bool in
      I.Bool b
    | "Int" ->
      one_of
        [
          ( "int_ast_string",
            let+ i = string in
            I.Int (Z.of_string i) );
          ( "int_as_int",
            let+ i = int in
            I.Int (Z.of_int i) );
        ]
    | "String" ->
      (* Assuming for older strings here that quotes were used for encoding before *)
      let+ s = string in
      if CCString.length s > 0 && CCOption.is_none !version then
        if s.[0] = '"' then
          I.String (CCString.sub s 1 (CCString.length s - 2))
        else
          I.String s
      else
        I.String s
    | "Float" ->
      one_of
        [
          ( "float_as_string",
            let+ q = string in
            I.Float (Q.of_string q) );
          ( "float_as_float",
            let+ q = float in
            I.Float (Q.of_float q) );
        ]
    | "Coll" ->
      let* ct = field "coll_type" coll_type_decoder in
      let+ l =
        field "args"
          (list
             (one_of
                [
                  "record_item", record_item_decoder ();
                  ( "expr",
                    let+ e = expr_decoder () in
                    I.Rec_value e );
                ]))
      in
      I.Coll (ct, l)
    | "MapColl" ->
      let* d = field "default" (record_item_decoder ()) in
      let+ c = field "elements" (list (record_item_pair_decoder ())) in
      I.MapColl (d, c)
    | "None" -> succeed I.LiteralNone
    | "Some" ->
      let+ e =
        one_of
          [
            "record_item", record_item_decoder ();
            ( "expr",
              let+ e = expr_decoder () in
              I.Rec_value e );
          ]
      in
      I.LiteralSome e
    | "Datetime" ->
      let+ d = datetime_decoder in
      I.Datetime d
    | s -> fail @@ "unrecognised literal: " ^ s)

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
        field "obj"
          (one_of
             [
               "record_item", record_item_decoder ();
               ( "expr",
                 let+ e = expr_decoder () in
                 I.Rec_value e );
             ])
      in
      let* index = field "index" (nullable string) in
      let* prop = field "prop" string in
      succeed
        (I.ObjectProperty { obj; index = CCOption.map Z.of_string index; prop })
    | "Funcall" ->
      let* func = field "name" string in
      let* args =
        field "args"
          (list
             (one_of
                [
                  "record_item", record_item_decoder ();
                  ( "expr",
                    let+ e = expr_decoder () in
                    I.Rec_value e );
                ]))
      in
      succeed (I.Funcall { func; args })
    | "CaseSplit" ->
      let* default_value = field "default_value" (record_item_decoder ()) in
      let+ cases =
        field "cases"
          (list
             (let* check = field "check" (record_item_decoder ()) in
              let+ value = field "value" (record_item_decoder ()) in
              check, value))
      in
      I.CaseSplit { default_value; cases }
    | "DataSetValue" ->
      let* name = field "name" string in
      let* field_name = field "field_name" string in
      let* default = field "default" (record_item_decoder ()) in
      let+ constraints = field "constraints" (list (record_item_decoder ())) in
      I.DataSetValue { name; field_name; default; constraints }
    | "Hof" ->
      let* hof_type = field "hof_type" hof_type_decoder in
      let* lambda_args = field "lambda_args" (list (value_decoder ())) in
      let+ body = field "body" (record_item_decoder ()) in
      I.Hof { hof_type; lambda_args; body }
    | s -> fail @@ "unrecognised value:" ^ s)

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
          [
            "record_item", field "lhs" (record_item_decoder ());
            ( "expr",
              field "lhs" (expr_decoder () >>= fun e -> succeed (I.Rec_value e))
            );
          ]
      in
      let+ rhs =
        one_of
          [
            "record_item", field "rhs" (record_item_decoder ());
            ( "expr",
              field "rhs" (expr_decoder () >>= fun e -> succeed (I.Rec_value e))
            );
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
        if CCString.length op >= 1 then
          (CCString.sub op 0 1).[0]
        else
          '+'
      in
      let* rhs = field "rhs" (expr_decoder ()) in
      succeed (I.Add { lhs; op; rhs })
    | "Mul" ->
      let* lhs = field "lhs" (expr_decoder ()) in
      let* op = field "op" string in
      let op =
        if CCString.length op >= 1 then
          (CCString.sub op 0 1).[0]
        else
          '*'
      in
      let* rhs = field "rhs" (expr_decoder ()) in
      succeed (I.Mul { lhs; op; rhs })
    | "In" ->
      let* el = field "el" (expr_decoder ()) in
      let* set = field "set" (value_decoder ()) in
      succeed (I.In { el; set })
    | s -> fail @@ "unrecognised expr: " ^ s)

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
      let* message_template = field "message_template" (nullable string) in
      succeed
        (I.Rec_repeating_group
           { num_in_group_field; elements; name; message_template })
    | s -> fail @@ "unrecognised record_item: " ^ s)

and record_decoder () : I.record D.decoder =
  let open D in
  field "Record"
    (let* elements =
       field "elements"
         (list
            (let* name = field "name" string in
             let* record_item = field "record_item" (record_item_decoder ()) in
             succeed (name, record_item)))
     in
     let* name = field "name" string in
     succeed I.{ name; elements = String_map.of_list elements })

let expecting_decoder : I.expecting D.decoder =
  let open D in
  one_of
    [
      ( "combined_expressions",
        let+ exprs = field "exprs" (list (expr_decoder ())) in
        I.
          {
            relevant_exprs = exprs;
            nullable_exprs = [];
            qe_modified_exprs = [];
            common_exprs = [];
          } );
      ( "structured_expressions",
        let* relevant_exprs = field "relevant_exprs" (list (expr_decoder ())) in
        let* common_exprs = field "common_exprs" (list (expr_decoder ())) in
        let* nullable_exprs =
          field "nullable_exprs"
            (one_of
               [
                 ( "single_nullable",
                   list
                     (let* field_paths =
                        field "field_paths" (list (list name_and_index_decoder))
                      in
                      let+ expr = field "expr" (expr_decoder ()) in
                      expr, CCList.map (fun x -> [ x ]) field_paths) );
                 ( "set_nullable",
                   list
                     (let* field_paths =
                        field "field_paths"
                          (list (list (list name_and_index_decoder)))
                      in
                      let+ expr = field "expr" (expr_decoder ()) in
                      expr, field_paths) );
               ])
        in
        let+ qe_modified_exprs =
          field "qe_modified_exprs" (list (expr_decoder ()))
        in
        I.{ relevant_exprs; nullable_exprs; qe_modified_exprs; common_exprs } );
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
      let* tag =
        one_of
          [
            "tag", field "tag" string;
            "template", field "template" (field "name" string);
          ]
      in
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
    | "Call" ->
      (* this is for older scripts *)
      let* prop = field "variable" string in
      let+ value = field "call" (value_decoder ()) in
      I.Set { prop; value = Rec_value (Value value) }
    | _ -> fail "unrecognised instruction")
