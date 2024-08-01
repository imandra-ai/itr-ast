let lit_string s = Itr_ast.(Literal (String s))

let lit_none = Itr_ast.(Literal LiteralNone)

let field_path_pp fmt = function
  | s, None -> CCFormat.fprintf fmt "%s" s
  | s, Some index -> CCFormat.fprintf fmt "%s[%i]" s (Z.to_int index)

let msg_pp =
  Itr_evaluator.Field_path_map.pp (CCList.pp field_path_pp) Itr_ast_pp.value_pp

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
                Rec_value
                  (Value (Field_path_map.get_or ~default:lit_none path ctx)));
          };
      field_presence_map = Field_path_map.empty;
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
      [ field_path, lit_string "non_default" ]
  in
  let ctx = context msg in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Field used when set in implicit message:@ @[input: %a@]@ \
     @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result;
  let msg =
    Itr_evaluator.Field_path_map.of_list
      [ [ "a_different_field", None ], lit_string "non_default" ]
  in
  let ctx = context msg in
  let result = Itr_evaluator.evaluate_expr ctx item in
  CCFormat.printf
    "@[<v 2>Default used when implicit message doesn't include field:@ \
     @[input: %a@]@ @[context: %a@]@ @[result: %a@] @]@."
    Itr_ast_pp.expr_pp item msg_pp msg Itr_ast_pp.record_item_pp result
