module I = Itr_ast

let s str = I.Rec_value (I.string str)

let ( = ) lhs rhs = I.Eq { lhs; rhs }

module P = Itr_ast_pp

let repeating_group_record =
  let open I in
  {
    name = "Parties";
    elements =
      String_map.of_list
        [
          ( "Parties",
            Rec_repeating_group
              {
                message_template = None;
                name = "Parties";
                num_in_group_field = "NoPartyIDs";
                elements =
                  [
                    {
                      I.name = "Parties";
                      elements =
                        String_map.of_list
                          [
                            "PartyID", s "1";
                            "Something", s "2";
                            ( "Instrument",
                              Rec_record
                                {
                                  name = "Instrument";
                                  elements =
                                    String_map.of_list [ "Symbol", s "UK123" ];
                                } );
                          ];
                    };
                    {
                      name = "Parties";
                      elements =
                        String_map.of_list
                          [
                            "PartyID", s "2";
                            "Something", s "3";
                            ( "SubGrp",
                              Rec_repeating_group
                                {
                                  name = "PartiesSubGrp";
                                  message_template = None;
                                  num_in_group_field = "NoSubIDs";
                                  elements =
                                    [
                                      {
                                        name = "PartiesSubGrp";
                                        elements =
                                          String_map.of_list [ "SubID", s "23" ];
                                      };
                                    ];
                                } );
                          ];
                    };
                  ];
              } );
        ];
  }

let () =
  CCFormat.printf "@[<v 2>Record with repeating group:@ %a@]@.@." P.record_pp
    repeating_group_record

let () =
  CCFormat.printf "@[<v 2>Receive:@ %a@]@.@." P.instruction_pp
    I.(
      receive ~variable:"msg4" ~where:(bool true)
        ~example:{ name = "msg4"; elements = String_map.empty }
        ~expecting:
          (expecting
             [
               Rec_value (message_value [ "ClOrdID" ]) = Rec_value (string "");
               Rec_value (message_value [ "CxlRejResponseTo" ])
               = Rec_value (string "1");
               Rec_value (message_value [ "OrdStatus" ])
               = Rec_value (string "0");
             ]
             [] [] [])
        ())

let () =
  CCFormat.printf "@[<v 2>expecting (no var):@ %a@]@.@." P.instruction_pp
    I.(
      receive ~variable:"msg4"
        ~example:{ name = "msg4"; elements = String_map.empty }
        ~where:(Rec_value (message_value [ "MsgType" ]) = Rec_value (string "8"))
        ~expecting:
          (expecting
             [ Rec_value (message_value [ "ClOrdID" ]) = Rec_value (string "") ]
             [] [] [])
        ())

let record =
  repeating_group_record |> fun { name; elements } ->
  I.
    {
      name;
      elements =
        I.String_map.add "Instrument"
          (I.Rec_record
             {
               name = "Instrument";
               elements =
                 I.String_map.of_list
                   [ "Symbol", I.Rec_value (I.string "GB123") ];
             })
          elements;
    }

let () =
  CCFormat.(
    printf "@[<v 2>flatten_record:@ %a@]@.@."
      (vbox (list (hbox (pair (list string) ~sep:(return " -> ") P.expr_pp))))
      (I.flatten_record record))

let () =
  CCFormat.(
    printf "@[<v 2>flatten_record ~qualify_records=false:@ %a@]@.@."
      (vbox (list (hbox (pair (list string) ~sep:(return " -> ") P.expr_pp))))
      (I.flatten_record ~qualify_records:false record))

let () =
  CCFormat.(
    printf "@[<v 2>concat:@ %a@]@.@." P.expr_pp
      (I.concat ~sep:(I.string ", ")
         [ I.string "one"; I.string "two"; I.string "three" ]))
