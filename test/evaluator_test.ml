let lit_string s = Itr_ast.(Literal (String s))

let lit_int i = Itr_ast.(Literal (Int i))

let lit_none = Itr_ast.(Literal LiteralNone)

let field_path_pp fmt = function
  | s, None -> CCFormat.fprintf fmt "%s" s
  | s, Some index -> CCFormat.fprintf fmt "%s[%i]" s (Z.to_int index)

let msg_pp =
  Itr_evaluator.Field_path_map.pp (CCList.pp field_path_pp)
    Itr_ast_pp.record_item_pp

let local_var_pp fmt lv =
  match lv with
  | Itr_evaluator.Record_item ri ->
    CCFormat.fprintf fmt "%a" Itr_ast_pp.record_item_pp ri
  | Itr_evaluator.Msg { msg; _ } -> CCFormat.fprintf fmt "%a" msg_pp msg

let local_vars_pp fmt
    (lvs :
      (string
      * Itr_ast.record_item Itr_evaluator.Field_path_map.t
        Itr_evaluator.msg_or_expr)
      list) =
  CCFormat.(
    fprintf fmt "%a"
      (list ~sep:(return "@,") (pair ~sep:(return ",") string local_var_pp))
      lvs)

let context msg local_vars =
  Itr_evaluator.
    {
      static_context =
        Some
          {
            local_vars = Itr_ast.String_map.of_list local_vars;
            implicit_message = Some { tag = "Message"; msg };
            get_field =
              (fun ctx path ->
                let ctx =
                  match ctx with
                  | Msg { msg = ctx; _ } -> ctx
                  | Record_item ctx ->
                    (match ctx with
                    | Rec_record { elements; _ } ->
                      Field_path_map.of_list
                        (Itr_ast.String_map.to_list elements
                        |> CCList.map (fun (s, e) -> [ s, None ], e))
                    | _ -> Field_path_map.empty)
                in
                Field_path_map.get_or ~default:(Rec_value (Value lit_none)) path
                  ctx);
          };
      field_presence_map =
        Field_path_map.map (fun _ -> Itr_evaluator.Present) msg;
    }

let () =
  let field_path = [ "field", None ] in
  let item =
    Itr_ast.(
      Value
        (Funcall
           {
             func = lit_string "defaultIfNotSet";
             args =
               [
                 Rec_value (Value (lit_string "default"));
                 Rec_value (Value (MessageValue { var = None; field_path }));
               ];
           }))
  in
  let msg =
    Itr_evaluator.Field_path_map.of_list
      [ field_path, Itr_ast.Rec_value (Value (lit_string "non_default")) ]
  in
  let ctx = context msg [] in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Field used when set in implicit message:@ @[input: %a@]@ \
     @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result;
  let msg =
    Itr_evaluator.Field_path_map.of_list
      [
        ( [ "a_different_field", None ],
          Itr_ast.Rec_value (Value (lit_string "non_default")) );
      ]
  in
  let ctx = context msg [] in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Default used when implicit message doesn't include field:@ \
     @[input: %a@]@ @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result

let () =
  let field_path_one = [ "field_one", None ] in
  let field_path_two = [ "field_two", None ] in
  let item =
    Itr_ast.(
      Value
        (Funcall
           {
             func = lit_string "defaultIfNotSet";
             args =
               [
                 Rec_value
                   (Value
                      (MessageValue { var = None; field_path = field_path_two }));
                 Rec_value
                   (Value
                      (MessageValue { var = None; field_path = field_path_one }));
               ];
           }))
  in
  let msg =
    Itr_evaluator.Field_path_map.of_list
      [
        field_path_one, Itr_ast.Rec_value (Value (lit_string "field_one"));
        field_path_two, Itr_ast.Rec_value (Value (lit_string "field_two"));
      ]
  in
  let ctx = context msg [] in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Result evaluated and used when there's a fallback default: @ \
     @[input: %a@]@ @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result

let () =
  let field_path_one = [ "field_one", None ] in
  let field_path_two = [ "field_two", None ] in
  let var_field_one = "var_field_1" in
  let var_field_two = "var_field_2" in
  let item =
    Itr_ast.(
      Rec_value
        (Eq
           {
             lhs =
               Rec_value
                 (Value
                    (ObjectProperty
                       {
                         obj = Rec_value (Value (Variable "event_1"));
                         index = None;
                         prop = var_field_one;
                       }));
             rhs = Rec_value (Value (Literal (Int Z.one)));
           }))
  in
  let msg =
    Itr_evaluator.Field_path_map.of_list
      [
        field_path_one, Itr_ast.Rec_value (Value (lit_string "field_one"));
        field_path_two, Itr_ast.Rec_value (Value (lit_string "field_two"));
      ]
  in
  let local_vars : (string * 'a Itr_evaluator.msg_or_expr) list =
    [
      ( "event_1",
        Itr_evaluator.(
          Record_item
            (Rec_record
               {
                 name = "event_1";
                 elements =
                   Itr_ast.String_map.of_list
                     [
                       ( var_field_one,
                         Itr_ast.Rec_value
                           (Value
                              (ObjectProperty
                                 {
                                   obj = Rec_value (Value (Variable "event_1"));
                                   index = None;
                                   prop = var_field_two;
                                 })) );
                       var_field_two, Itr_ast.Rec_value (Value (lit_int Z.one));
                     ];
               })) );
    ]
  in
  let ctx = context msg local_vars in
  let result = Itr_evaluator.fix_evaluate_record_item ctx item in
  CCFormat.printf
    "@[<v 2>Result evaluated with a variable with self reference: @ @[input: \
     %a@]@ @[local vars: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.record_item_pp item local_vars_pp local_vars
    Itr_ast_pp.record_item_pp result

let () =
  let field_path_one = [ "field_two", None ] in
  let item =
    Itr_ast.(
      Eq
        {
          lhs = Rec_value (Value (Literal LiteralNone));
          rhs =
            Rec_value
              (Value (MessageValue { var = None; field_path = field_path_one }));
        })
  in
  let result = Itr_evaluator.evaluate_expr Itr_evaluator.empty_context item in
  CCFormat.printf
    "@[<v 2>Result evaluated from equality to field path to: @ @[input: %a@]@ \
     @[context: empty@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item Itr_ast_pp.record_item_pp result

let () =
  let item =
    Itr_ast.(
      Eq
        {
          lhs =
            Rec_value
              (Value
                 (Literal
                    (LiteralSome (Rec_value (Value (Literal (String "1")))))));
          rhs = Rec_value (Value (Literal (String "1")));
        })
  in
  let result = Itr_evaluator.evaluate_expr Itr_evaluator.empty_context item in
  CCFormat.printf
    "@[<v 2>Result evaluated from equality of FIX contextual terms: @ @[input: %a@]@ \
     @[context: empty@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item Itr_ast_pp.record_item_pp result
