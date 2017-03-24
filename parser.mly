/* Ocamlyacc parser for Theatr */

%{
open Ast
%}

%token PUNC_SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token LANGLE RANGLE ASSIGN
%token ARITH_PLUS ARITH_MINUS ARITH_TIMES ARITH_DIVIDE ARITH_MOD
%token PUNC_COMMA PUNC_SEMI PUNC_PERIOD PUNC_COLON
%token LOGIC_EQ LOGIC_NEQ LOGIC_LT LOGIC_GT LOGIC_LEQ LOGIC_GEQ LOGIC_AND LOGIC_OR LOGIC_TRUE LOGIC_FALSE LOGIC_NOT
%token BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_RIGHT BITWISE_LEFT
%token FUNC_ARROW FUNC_RETURN FUNC_DECL
%token FLOW_IF FLOW_ELSE FLOW_BREAK FLOW_CONTINUE FLOW_FOR FLOW_WHILE
%token CASE MATCH WILDCARD
%token ACT_NEW ACT_RECEIVE ACT_DROP ACT_AFTER ACT_SEND ACT_SENDER
%token TYPE_INT TYPE_FLOAT TYPE_CHAR TYPE_STR TYPE_BOOL TYPE_NONE
%token TYPE_LIST TYPE_ARRAY TYPE_SET TYPE_DICT TYPE_TUPLE TYPE_STRUCT TYPE_ACTOR
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STR_LIT
%token <string> ID
%token EOF

%nonassoc FLOW_NOELSE
%nonassoc FLOW_ELSE
%right ASSIGN
%left LOGIC_OR
%left LOGIC_AND
%left LOGIC_EQ LOGIC_NEQ
%left LOGIC_LEQ LOGIC_GEQ LOGIC_GT LOGIC_LT
%left ARITH_PLUS ARITH_MINUS
%left ARITH_TIMES ARITH_DIVIDE
%left ARITH_MOD
%right LOGIC_NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

/* same as MicroC */
decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

/* differs from MicroC in that we allow variable declarations
   and statements to be intertwined, so no need to separate out
   vdecl_list and stmt_list. stmt_list can hold vdecls
*/
fdecl:
   FUNC_DECL ID LPAREN formals_opt RPAREN FUNC_ARROW typ PUNC_COLON LBRACE stmt_list RBRACE
     { { typ = $7;
	 fname = $2;
	 formals = $4;
	 body = List.rev $10 } }

/* same as MicroC */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

/* same as MicroC */
formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list PUNC_COMMA typ ID { ($3,$4) :: $1 }

typ:
    ptyp { $1 }
  | ctyp { $1 }

ptyp:
    TYPE_INT { Int }
  | TYPE_FLOAT { Float }
  | TYPE_BOOL { Bool }
  | TYPE_CHAR { Char }
  | TYPE_STR { String }
  | TYPE_ACTOR { Actor }
  | TYPE_TUPLE { Tuple }
  | TYPE_STRUCT { Struct }
  | TYPE_NONE { None }

ctyp:
    TYPE_LIST ptyp { ($1, $2) }
  | TYPE_ARRAY ptyp { ($1, $2) }
  | TYPE_SET ptyp { ($1, $2) }
  | TYPE_DICT ptyp ptyp { ($1, $2, $3) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* added vdecls as possible elements of stmt_list */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }
  | stmt_list vdecl { $2 :: $1 }  

/* small syntax changes to if/else/for/while */
stmt:
    expr PUNC_SEMI { Expr $1 }
  | FUNC_RETURN PUNC_SEMI { Return Noexpr }
  | FUNC_RETURN expr PUNC_SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | FLOW_IF LPAREN expr RPAREN PUNC_COLON stmt %prec FLOW_NOELSE { If($3, $6, Block([])) }
  | FLOW_IF LPAREN expr RPAREN PUNC_COLON stmt FLOW_ELSE PUNC_COLON stmt { If($3, $6, $9) }
  | FLOW_FOR LPAREN expr_opt PUNC_SEMI expr PUNC_SEMI expr_opt RPAREN PUNC_COLON stmt
     { For($3, $5, $7, $10) }
  | FLOW_WHILE LPAREN expr RPAREN PUNC_COLON stmt { While($3, $6) }

/* same as MicroC */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LIT	           { IntLiteral($1) }
  | FLOAT_LIT		   { FloatLiteral($1) }
  | STR_LIT		   { StrLiteral($1) }
  | LOGIC_TRUE		   { BoolLit(true) }
  | LOGIC_FALSE		   { BoolLit(false) }
  | ID   	           { Id($1) }
  | expr ARITH_PLUS   expr { Binop($1, Add,   $3) }
  | expr ARITH_MINUS  expr { Binop($1, Sub,   $3) }
  | expr ARITH_TIMES  expr { Binop($1, Mult,  $3) }
  | expr ARITH_DIVIDE expr { Binop($1, Div,   $3) }
  | expr LOGIC_EQ     expr { Binop($1, Equal, $3) }
  | expr LOGIC_NEQ    expr { Binop($1, Neq,   $3) }
  | expr LOGIC_LT     expr { Binop($1, Less,  $3) }
  | expr LOGIC_LEQ    expr { Binop($1, Leq,   $3) }
  | expr LOGIC_GT     expr { Binop($1, Greater, $3) }
  | expr LOGIC_GEQ    expr { Binop($1, Geq,   $3) }
  | expr LOGIC_AND    expr { Binop($1, And,   $3) }
  | expr LOGIC_OR     expr { Binop($1, Or,    $3) }
  | ARITH_MINUS expr %prec NEG { Unop(Neg, $2) }
  | LOGIC_NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

/* same as MicroC */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

/* same as MicroC */
actuals_list:
    expr                    { [$1] }
  | actuals_list PUNC_COMMA expr { $3 :: $1 }
