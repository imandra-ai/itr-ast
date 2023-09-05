module D = Decoders_yojson.Basic.Decode

let run_test filename =
  let cwd = Sys.getcwd () in
  if String.length filename < 6 then failwith "not long enough to be a json file name" else
  let json_ext =  String.sub filename ((String.length filename) - 4) 4 in
  if json_ext <> "json" then failwith "Not a json extension"
  else let base_filename = String.sub filename 0 (String.length filename - 5) in
  print_endline filename;
  let instructions = D.decode_file (D.field "instructions" (D.list (Itr_ast_decoder.instruction_decoder ())))
  (CCFormat.sprintf "%s/%s" cwd filename) in
  match instructions with
  | Ok instructions ->
      let pp_string = CCFormat.sprintf "%a" Itr_ast_pp.instructions_pp instructions in
      let filename = CCFormat.sprintf "%s.out" base_filename in
      let oc = open_out filename in
      Printf.fprintf oc "%s\n" pp_string;
      close_out oc
  | Error e -> failwith (D.string_of_error e)

let () = run_test "simple_limit.json"

let () = run_test "test1.json"

let () = run_test "test2.json"

let () = run_test "test3.json"

