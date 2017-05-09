(* Ocamllex scanner for Theatr *)

{
    open Parser

    let make_char c =
        let ec = Char.escaped c in
        let s = Scanf.unescaped (match String.length ec with
            1 -> "\\" ^ ec
          | _ -> ec) in
        String.get s 0
}

let digit = ['0'-'9']
let double = ('-'?)((digit+'.'digit*) | ('.'digit+))
let simp_chr = [' '-'!' '#'-'&' '('-'[' ']'-'~']


let esc_chr = ['t' 'n' 'r' '\'' '\"' '\\']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { single_line_comment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| "receive" { RECEIVE }
| "drop"   { DROP }
| "after"  { AFTER }
| "->"     { FUNC_ARROW }
| "func"   { FUNC_DECL }
| "new"    { NEW }
| "actor"  { ACTOR }
| "struct" { STRUCT }
| '.'      { DOT }
| '|'      { PIPE }
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
| "list"   { LIST }
| "array"  { ARRAY }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "double" { DOUBLE }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "char"   { CHAR }
| "string" { STRING }

(* Literals *)
| digit+ as lxm { INTLIT(int_of_string lxm) }
| double as lxm { DOUBLELIT(float_of_string lxm)}
| '\'' (simp_chr as lxm) '\'' { CHARLIT(lxm) }
| "'\\" (esc_chr as ec) "'" { CHARLIT(make_char ec) }
| '"'      { read_string (Buffer.create 17) lexbuf }

(* Identifiers *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(* From: realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
and read_string buf =
  parse
  | '"'       { STRINGLIT (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"' { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
    
  | _ as char { raise (Failure("illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure("String is not terminated")) }


and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and single_line_comment = parse
| '\n'  { token lexbuf }
| _     { single_line_comment lexbuf }
