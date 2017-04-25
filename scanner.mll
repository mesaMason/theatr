(* Ocamllex scanner for Theatr *)

{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let punct = [' '-'!' '#'-'[' ']'-'~']
let str = (letter | digit | punct)* as s

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| "->"     { FUNC_ARROW }
| "func"   { FUNC_DECL }
| ':'      { COLON }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "none"   { NONE }
| "string" { STRING }
| "list"   { LIST }
| "array"    { ARRAY }
| "new"    { NEW }
| "actor"  { ACTOR }
| "receive" { RECEIVE }
| "drop"    { DROP}
| "after"   { AFTER }
| "true"   { TRUE }
| "false"  { FALSE }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| '"' str '"' { STRINGLIT(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
