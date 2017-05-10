open Ast
open Semant

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let astree = Parser.program Scanner.token lexbuf in
  Semant.check astree;
  let result = Ast.string_of_program astree in
    print_string (result)
