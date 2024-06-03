let () =
  if Array.length Sys.argv < 2 then
    failwith "no argument specified"
  else
    Itr_ast_pp.gen_pp Sys.argv.(1)
