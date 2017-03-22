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
%token FLOW_IF FLOW_ELSE FLOW_BREAK FLOW_CONTINUE
%token CASE MATCH WILDCARD
%token ACT_NEW ACT_RECEIVE ACT_DROP ACT_AFTER ACT_SEND ACT_SENDER
%token TYPE_INT TYPE_FLOAT TYPE_CHAR TYPE_STR TYPE_BOOL TYPE_NONE
%token TYPE_LIST TYPE_ARRAY TYPE_SET TYPE_DICT TYPE_TUPLE TYPE_STRUCT TYPE_ACTOR
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc FLOW_ELSE
%right ASSIGN
%left LOGIC_OR
%left LOGIC_AND
%left LOGIC_EQ LOGIC_NEQ
%left LOGIC_LEQ LOGIC_GEQ
%left ARITH_PLUS ARITH_MINUS
%left ARITH_TIMES ARITH_DIVIDE
%left ARITH_MOD
%right LOGIC_NOT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
