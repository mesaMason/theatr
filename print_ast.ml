open Ast
       
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let astree = Parser.program Scanner.token lexbuf in
  let result = Ast.string_of_program astree in
  print_string (result)
