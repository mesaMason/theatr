(* Ocamllex scanner for Theatr *)

{ open Parser }

<<<<<<< f1ca37be3a9de246f81e31ca4633fe72d1ae39a0
let digit = ['0' - '9']
let double = ('-'?)((digit+'.'digit*) | ('.'digit+))


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"	   { comment_single lexbuf}

(* braces/parens/brackets *)
=======
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
>>>>>>> Changed file extension from mc to th
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
<<<<<<< f1ca37be3a9de246f81e31ca4633fe72d1ae39a0
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '<'      { LANGLE }
| '>'      { RANGLE }

| '='      { ASSIGN }

(* math *)
| '+'                   { ARITH_PLUS }
| '-'                   { ARITH_MINUS }
| '*'                   { ARITH_TIMES }
| '/'                   { ARITH_DIVIDE }
| '%'                   { ARITH_MOD }

(* comma, semi *)
| ','                   { PUNC_COMMA }
| ';'                   { PUNC_SEMI }
| '.'                   { PUNC_PERIOD }
| ':'                   { PUNC_COLON }

(* logic *)
| "=="                  { LOGIC_EQ }
| "!="                  { LOGIC_NEQ }
| "<="                  { LOGIC_LEQ }
| ">="                  { LOGIC_GEQ }
| "&&"                  { LOGIC_AND }
| "||"                  { LOGIC_OR }
| "true"                { LOGIC_TRUE }
| "false"               { LOGIC_FALSE }
| '!'                   { LOGIC_NOT }

(* bit math *)
| '&'                   { BITWISE_AND }
| '|'                   { BITWISE_OR }
| '^'                   { BITWISE_XOR }
| ">>"                  { BITWISE_RIGHT }
| "<<"                  { BITWISE_LEFT }

(* functions *)
| "->"                  { FUNC_RET }
| "return"              { RETURN }

(* flow control *)
| "if"                  { FLOW_IF }
| "else"                { FLOW_ELSE }
| "break"               { FLOW_BREAK }
| "continue"            { FLOW_CONTINUE }

(* matching *)
| "case"                { CASE }
| "match"               { MATCH }
| '_'                   { WILDCARD  }

(* actors *)
| "new"                 { ACT_NEW }
| "receive"             { ACT_RECEIVE }
| "drop"                { ACT_DROP }
| "after"               { ACT_AFTER }
| "|"                   { ACT_SEND }
| "Sender"              { ACT_SENDER }

(* primitive types *)
| "int"                 { TYPE_INT }
| "float"               { TYPE_FLOAT }
| "char"                { TYPE_CHAR }
| "string"              { TYPE_STR }
| "bool"                { TYPE_BOOL }
| "none"                { TYPE_NONE }

(* function declaration *)
| "func"                { FUNC_DECL }

(* non-primitive types *)
| "List"                { TYPE_LIST }
| "Array"               { TYPE_ARRAY }
| "Set"                 { TYPE_SET }
| "Dict"                { TYPE_DICT }
| "Tuple"               { TYPE_TUPLE }
| "Struct"              { TYPE_STRUCT }
| "Actor"               { TYPE_ACTOR }
		
(* literals *)
| digit+ as lxm { INT_LIT(int_of_string lxm) }
| double as lxm { DOUBLE_LIT(float_of_string lxm) }
=======
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
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
>>>>>>> Changed file extension from mc to th
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
<<<<<<< f1ca37be3a9de246f81e31ca4633fe72d1ae39a0

and comment_single = parse
| '\n' { token lexbuf }
|_ { comment lexbuf }
=======
>>>>>>> Changed file extension from mc to th
