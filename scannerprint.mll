{ open Printf }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { print_string "LPAREN " }
| ')'      { print_string "RPAREN " }
| '{'      { print_string "LBRACE " }
| '}'      { print_string "RBRACE " }
| ';'      { print_string "SEMI " }
| "int"    { print_string "INT " }
| ['0'-'9']+ { print_string "LITERAL " }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* { print_string "ID " }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            ignore (token lexbuf)
        done
    with _ -> print_string "invalid_token\n"
  let _ = Printexc.print main ()

}
