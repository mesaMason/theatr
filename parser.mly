/* Ocamlyacc parser for Theatr */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COLON LBRACKET RBRACKET
%token FUNC_DECL FUNC_ARROW
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL NONE STRING
%token LIST ARRAY
%token ACTOR RECEIVE DROP AFTER NEW
%token <int> INTLIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { List.rev $1 }

decls:
   /* nothing */ { [] }
 | decls stmt { (Stmt($2) :: $1) }
 | decls fdef { (Fdef($2) :: $1) }
 | decls adef { (Adef($2) :: $1) }
 
fdef:
   FUNC_DECL ID LPAREN formals_opt RPAREN FUNC_ARROW typ COLON LBRACE stmt_list_opt RBRACE
     { { formals = $4;
         rtyp = $7;
	 fname = $2;
	 body = $10 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }
 
formal_list:
    typ ID   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    ptyp { Ptyp($1) }
  | ctyp LT ptyp GT{ Ctyp($1, $3) }

ptyp:
    INT { Int }
  | BOOL { Bool }
  | NONE { None }
  | STRING { String }
  | ACTOR { Actor }
           
ctyp:
    LIST  { List }
  | ARRAY { Array }

stmt_list_opt:
    /* nothing */    { [] }
  | stmt_list { List.rev $1 }

stmt_list:
    stmt { [$1] }
  | stmt_list stmt { $2 :: $1 }
              
stmt:
    expr SEMI { Expr($1) }
  | vdecl { Vdecl($1) }
  | vdef  { Vdef($1) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN COLON stmt %prec NOELSE { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN COLON stmt ELSE COLON stmt    { If($3, $6, $9) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN COLON stmt
     { For($3, $5, $7, $10) }
  | WHILE LPAREN expr RPAREN COLON stmt { While($3, $6) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }

vdef:
    typ ID ASSIGN expr SEMI { ($1, $2, $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INTLIT          { IntLit($1) }
  | STRINGLIT        { StringLit($1) }  
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | LIST LBRACKET actuals_opt RBRACKET { ListC($3) }
  | ARRAY LBRACKET actuals_opt RBRACKET { ArrayC($3) }
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
  | ID ASSIGN expr { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | NEW ID LPAREN actuals_opt RPAREN { Acall($2, $4) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

adef:
   ID LPAREN formals_opt RPAREN COLON LBRACE vdecl_list receives drop after RBRACE
     { { aname = $1;
	 aformals = $3;
	 alocals = List.rev $7;
	 receives = $8;
	 drop = $9;
	 after = $10; } }

receives:
    /* nothing */ { [] }
  | RECEIVE COLON LBRACE msg_decl_list RBRACE { List.rev $4 }
     
msg_decl_list:
    /* nothing */ { [] }
  | msg_decl_list msg_decl { $2 :: $1 }		  

/* declaration of what an actor does upon receiving the message
structure is very similar to functions
*/
msg_decl:
    ID LPAREN formals_opt RPAREN COLON LBRACE stmt_list RBRACE	
      { { mname = $1;
	  mformals = $3;
	  mbody = List.rev $7 } }
	  
drop:
      /* nothing */ { { dabody = [] } }
    | DROP COLON LBRACE stmt_list RBRACE
      { { dabody = List.rev $4 } }

after:
      /* nothing */ { { dabody = [] } }
    | AFTER COLON LBRACE stmt_list RBRACE
      { { dabody = List.rev $4 } }
