(* Abstract Syntax Tree and functions for printing it

Authors:
Betsy Carroll
Suraj Keshri
Mike Lin
Linda Orgeta
*)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type ptyp = Int | Bool | Double | Void | Actor | String | Char

type ctyp = List | Array

type typ = Ptyp of ptyp | Ctyp of ctyp * ptyp
  
type bind = typ * string

type sdecl = {
    name : string;
    elements : bind list
  }

type expr =
    IntLit of int
  | DoubleLit of float
  | StringLit of string
  | CharLit of char
  | BoolLit of bool
  | ListC of expr list
  | ArrayC of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | NewActor of string * expr list
  | Send of string * string * expr list * string
  | Noexpr

type vdef = {
    vtyp : typ;
    vname : string;
    vvalue : expr;
  }

type sdef  = {
    sname : string;
    styp : string;
    svalue : vdef list;
  }
               
type stmt =
    Block of stmt list
  | Expr of expr
  | Vdecl of bind
  | Vdef of vdef
  | Sdef of sdef
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
    alocals : stmt list;
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
  | CharLit(c)   -> Char.escaped c
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | ArrayC(el) -> "Array" ^ "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | ListC(el) -> "List" ^ "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NewActor(a, el) ->
     "new" ^ a ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Send(aTyp, mname, el, a) -> 
          aTyp ^ "." ^ mname ^ String.concat ", " (List.map string_of_expr el) ^ ") | " ^ a 

let string_of_ptyp = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Actor -> "actor"
  | Double -> "double"
  | String -> "string"
  | Char    -> "char"

let string_of_ctyp = function
    List -> "list"
  | Array -> "array"

let string_of_typ = function
    Ptyp(e) -> string_of_ptyp e
  | Ctyp(c, e) -> string_of_ctyp c ^ "<" ^ string_of_ptyp e ^ ">"
               
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id

let string_of_vdef vdef =
  string_of_typ vdef.vtyp ^ " " ^ vdef.vname ^ " = " ^ string_of_expr vdef.vvalue

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Vdecl((t, id)) -> string_of_vdecl (t, id) ^ "\n";
  | Vdef(vdef) -> string_of_vdef vdef ^ ";\n"
  | Sdef(sdef) -> "struct " ^ sdef.sname ^ " = new " ^ sdef.styp ^ "()\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

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
  String.concat "" (List.map string_of_stmt adecl.alocals) ^
  "\nreceives:\n" ^ String.concat "" (List.map string_of_mdecl adecl.receives) ^
  "\ndrop:\n" ^ String.concat "" (List.map string_of_stmt adecl.drop.dabody) ^
  "\nafter:\n" ^ String.concat "" (List.map string_of_stmt adecl.after.dabody) ^
  "}\n"

let string_of_sdecl sdecl =
  "struct " ^ sdecl.name ^ " {\n" ^  String.concat "\n" (List.map string_of_vdecl sdecl.elements) ^ "\n}" 

let string_of_program (vars, funcs, actors) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_adecl actors) ^ "\n"

