{ open Printf }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { print_string "LPAREN " }
| ')'      { print_string "RPAREN "}
| '{'      { print_string "LBRACE "}
| '}'      { print_string "RBRACE "}
| "receive" {print_string "RECEIVE "}
| "drop"   { print_string "DROP "}
| "after"  { print_string "AFTER "}	   
| "->"     { print_string "FUNC_ARROW "}
| "func"   { print_string "FUNC_DECL "}
| ':'      { print_string "COLON "}
| ';'      { print_string "SEMI "}
| ','      { print_string "COMMA "}
| '+'      { print_string "PLUS "}
| '-'      { print_string "MINUS "}
| '*'      { print_string "TIMES "}
| '/'      { print_string "DIVIDE "}
| '='      { print_string "ASSIGN "}
| "=="     { print_string "EQ "}
| "!="     { print_string "NEQ "}
| '<'      { print_string "LT "}
| "<="     { print_string "LEQ "}
| ">"      { print_string "GT "}
| ">="     { print_string "GEQ "}
| "&&"     { print_string "AND "}
| "||"     { print_string "OR "}
| "!"      { print_string "NOT "}
| "if"     { print_string "IF "}
| "else"   { print_string "ELSE "}
| "for"    { print_string "FOR "}
| "while"  { print_string "WHILE "}
| "return" { print_string "RETURN "}
| "int"    { print_string "INT "}
| "bool"   { print_string "BOOL "}
| "void"   { print_string "VOID "}
| "true"   { print_string "TRUE "}
| "false"  { print_string "FALSE "}
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
