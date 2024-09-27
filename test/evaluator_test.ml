let lit_string s = Itr_ast.(Literal (String s))

let lit_none = Itr_ast.(Literal LiteralNone)

let field_path_pp fmt = function
  | s, None -> CCFormat.fprintf fmt "%s" s
  | s, Some index -> CCFormat.fprintf fmt "%s[%i]" s (Z.to_int index)

let msg_pp =
  Itr_evaluator.Field_path_map.pp (CCList.pp field_path_pp)
    Itr_ast_pp.record_item_pp

let context msg =
  Itr_evaluator.
    {
      static_context =
        Some
          {
            local_vars = Itr_ast.String_map.empty;
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
  let ctx = context msg in
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
  let ctx = context msg in
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
  let ctx = context msg in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Result evaluated and used when there's a fallback default: @ \
     @[input: %a@]@ @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result

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
