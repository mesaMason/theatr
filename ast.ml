(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Double | Void | String | Actor

type bind = typ * string

type expr =
    IntLit of int
  | DoubleLit of float
  | StringLit of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | NewActor of string * expr list
  | Noexpr

type vdef = {
    vtyp : typ;
    vname : string;
    vvalue : expr;
  }

type stmt =
    Block of stmt list
  | Expr of expr
  | Vdecl of bind
  | Vdef of vdef
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type msg_decl = {
    mname : string;
    mformals : bind list;
    mbody : stmt list;
  }
		   
type drop_after_decl = {
    dabody : stmt list;
  }

type actor_decl = {
    aname : string;
    aformals : bind list;
    alocals : bind list;
    receives : msg_decl list;
    drop : drop_after_decl;
    after : drop_after_decl;
  }
		   
type program = bind list * func_decl list * actor_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | DoubleLit(f) -> string_of_float f
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NewActor(a, el) ->
      a ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let string_of_typ = function
    Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"
  | Actor -> "actor"
               
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl((t, id)) -> string_of_vdecl (t, id);
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_mdecl mdecl =
  mdecl.mname ^ "(" ^ String.concat ", " (List.map snd mdecl.mformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt mdecl.mbody) ^
  "}\n"

let string_of_adecl adecl =
  adecl.aname ^ "(" ^ String.concat ", " (List.map snd adecl.aformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl adecl.alocals) ^
  "\nreceives:\n" ^ String.concat "" (List.map string_of_mdecl adecl.receives) ^
  "\ndrop:\n" ^ String.concat "" (List.map string_of_stmt adecl.drop.dabody) ^
  "\nafter:\n" ^ String.concat "" (List.map string_of_stmt adecl.after.dabody) ^
  "}\n"

let string_of_program (vars, funcs, actors) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_adecl actors) ^ "\n"
