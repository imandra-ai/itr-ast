open Format
open Itr_ast
open Yojson.Safe
module JU = Yojson.Basic.Util

let span_to_json (s : T.span) =
  let d, ps = T.Span.to_d_ps s in
  `List [ `String (Z.to_string d); `String (Z.to_string ps) ]

let ptime_to_json (t : T.t) = span_to_json (T.to_span t)

let datetime_to_json : datetime -> t = function
  | UTCTimestamp d -> `Assoc [ "UTCTimestamp", ptime_to_json d ]
  | UTCTimeOnly d -> `Assoc [ "UTCTimeOnly", ptime_to_json d ]
  | UTCDateOnly d -> `Assoc [ "UTCDateOnly", ptime_to_json d ]
  | LocalMktDate d -> `Assoc [ "LocalMktDate", ptime_to_json d ]
  | MonthYear (d, w) ->
    `Assoc
      [
        ( "MonthYear",
          `Assoc
            [
              "t", ptime_to_json d;
              ( "week",
                match w with
                | None -> `Null
                | Some w -> `String (Itr_ast_pp.week_to_string w) );
            ] );
      ]
  | Duration d -> `Assoc [ "Duration", span_to_json d ]

let hof_type_to_json : hof_type -> t = function
  | For_all -> `String "For_all"
  | Exists -> `String "Exists"
  | Map -> `String "Map"
  | Filter -> `String "Filter"
  | Find -> `String "Find"
  | For_all2 -> `String "For_all2"
  | Map2 -> `String "Map2"

let coll_type_to_json : coll_type -> t = function
  | Tuple -> `String "Tuple"
  | Set -> `String "Set"
  | List -> `String "List"

let rec literal_to_json : literal -> t = function
  | Bool b -> `Assoc [ "Bool", `Bool b ]
  | Int i -> `Assoc [ "Int", `String (Z.to_string i) ]
  | String s -> `Assoc [ "String", `String s ]
  | Float q -> `Assoc [ "Float", `String (Q.to_string q) ]
  | Coll (ct, l) ->
    `Assoc
      [
        ( "Coll",
          `Assoc
            [
              "coll_type", coll_type_to_json ct;
              "args", `List (List.map record_item_to_json l);
            ] );
      ]
  | MapColl (d, l) ->
    `Assoc
      [
        ( "MapColl",
          `Assoc
            [
              "default", record_item_to_json d;
              "elements", `List (List.map record_item_pair_to_json l);
            ] );
      ]
  | LiteralNone -> `Assoc [ "None", `String "None" ]
  | LiteralSome e -> `Assoc [ "Some", record_item_to_json e ]
  | Datetime d -> `Assoc [ "Datetime", datetime_to_json d ]

and record_item_pair_to_json : record_item * record_item -> t = function
  | e1, e2 ->
    `Assoc [ "key", record_item_to_json e1; "value", record_item_to_json e2 ]

and value_to_json : value -> t = function
  | Literal l -> `Assoc [ "Literal", literal_to_json l ]
  | Variable v -> `Assoc [ "Variable", `String v ]
  | LambdaVariable v -> `Assoc [ "LambdaVariable", `String v ]
  | MessageValue mv -> `Assoc [ "Message_value", Message_value.to_json mv ]
  | ObjectProperty op ->
    `Assoc
      [
        ( "ObjectProperty",
          `Assoc
            [
              "obj", record_item_to_json op.obj;
              "index", opt_index_to_json op.index;
              "prop", `String op.prop;
            ] );
      ]
  | Funcall { func; args } ->
    `Assoc
      [
        ( "Funcall",
          `Assoc
            [
              "name", value_to_json func;
              "args", `List (List.map record_item_to_json args);
            ] );
      ]
  | CaseSplit { default_value; cases } ->
    `Assoc
      [
        ( "CaseSplit",
          `Assoc
            [
              "default_value", record_item_to_json default_value;
              ( "cases",
                `List
                  (List.map
                     (fun (c, v) ->
                       `Assoc
                         [
                           "check", record_item_to_json c;
                           "value", record_item_to_json v;
                         ])
                     cases) );
            ] );
      ]
  | DataSetValue { name; field_name; default; constraints } ->
    `Assoc
      [
        ( "DataSetValue",
          `Assoc
            [
              "name", `String name;
              "field_name", `String field_name;
              "default", record_item_to_json default;
              "constraints", `List (List.map record_item_to_json constraints);
            ] );
      ]
  | Hof { hof_type; lambda_args; body } ->
    `Assoc
      [
        ( "Hof",
          `Assoc
            [
              "hof_type", hof_type_to_json hof_type;
              "lambda_args", `List (List.map value_to_json lambda_args);
              "body", record_item_to_json body;
            ] );
      ]

and opt_index_to_json : Z.t option -> t = function
  | None -> `Null
  | Some i -> `String (Z.to_string i)

and expr_to_json : expr -> t = function
  | Value v -> `Assoc [ "Value", value_to_json v ]
  | Not e -> `Assoc [ "Not", expr_to_json e ]
  | Or { lhs; rhs } ->
    `Assoc [ "Or", `Assoc [ "lhs", expr_to_json lhs; "rhs", expr_to_json rhs ] ]
  | And { lhs; rhs } ->
    `Assoc
      [ "And", `Assoc [ "lhs", expr_to_json lhs; "rhs", expr_to_json rhs ] ]
  | Eq { lhs; rhs } ->
    `Assoc
      [
        ( "Eq",
          `Assoc
            [ "lhs", record_item_to_json lhs; "rhs", record_item_to_json rhs ] );
      ]
  | Cmp { lhs; op; rhs } ->
    `Assoc
      [
        ( "Cmp",
          `Assoc
            [
              "lhs", expr_to_json lhs; "op", `String op; "rhs", expr_to_json rhs;
            ] );
      ]
  | Add { lhs; op; rhs } ->
    `Assoc
      [
        ( "Add",
          `Assoc
            [
              "lhs", expr_to_json lhs;
              "op", `String (Char.escaped op);
              "rhs", expr_to_json rhs;
            ] );
      ]
  | Mul { lhs; op; rhs } ->
    `Assoc
      [
        ( "Mul",
          `Assoc
            [
              "lhs", expr_to_json lhs;
              "op", `String (Char.escaped op);
              "rhs", expr_to_json rhs;
            ] );
      ]
  | In { el; set } ->
    `Assoc [ "In", `Assoc [ "el", expr_to_json el; "set", value_to_json set ] ]

and record_to_json (record : record) =
  let item_to_json ((name : string), (item : record_item)) =
    `Assoc [ "name", `String name; "record_item", record_item_to_json item ]
  in
  `Assoc
    [
      ( "Record",
        `Assoc
          [
            "name", `String record.name;
            ( "elements",
              `List
                (List.map item_to_json
                   (String_map.to_list record.elements |> CCList.rev)) );
          ] );
    ]

and record_item_to_json (item : record_item) =
  match item with
  | Rec_value value -> `Assoc [ "Rec_value", expr_to_json value ]
  | Rec_record items -> `Assoc [ "Rec_record", record_to_json items ]
  | Rec_repeating_group { num_in_group_field; elements; name; message_template }
    ->
    `Assoc
      [
        ( "Rec_repeating_group",
          `Assoc
            [
              "num_in_group_field", `String num_in_group_field;
              "elements", `List (List.map record_to_json elements);
              "name", `String name;
              ( "message_template",
                match message_template with
                | None -> `Null
                | Some message_template -> `String message_template );
            ] );
      ]
