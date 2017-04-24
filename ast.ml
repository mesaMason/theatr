(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

(* TODO: Lambda *)
type ptyp = Int | Bool | None | String

type ctyp = List | Array 
(*type ftyp = Func *)

type typ = Ptyp of ptyp | Ctyp of ctyp * ptyp(* | Ftyp of ftyp*)

type vdecl = typ * string
                                                   
type expr =
    IntLit of int
  | StringLit of string
  | BoolLit of bool
  | ArrayC of expr list
  | ListC of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Expr of expr
  | Block of stmt list
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

(*                      
type Func = {
    atyp: typ list;
    rtyp: typ;
}

 *)
type vdef = typ * string * expr

type entry =
    Vdecl of vdecl
  | Vdef of vdef
  | Stmt of stmt

type fdef = {
    formals : vdecl list;
    rtyp : typ;
    fname : string;
    body : entry list;
}

type bind =
    Entry of entry 
  | Fdef of fdef
                  
type program = bind list
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
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayC(el) -> "Array" ^ "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | ListC(el) -> "List" ^ "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ "; " ^ string_of_expr e2 ^ "; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")" ^ string_of_stmt s

let string_of_ptyp = function
    Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | None -> "none"

let string_of_ctyp = function
    List -> "list"
  | Array -> "array"

let string_of_typ = function
    Ptyp(e) -> string_of_ptyp e
  | Ctyp(c, e) -> string_of_ctyp c ^ "<" ^ string_of_ptyp e ^ ">"

let string_of_vdecl (t, s) = string_of_typ t ^ " " ^ s ^ ";\n"

let string_of_vdef (t, s, e) = string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
                                                                                        
let string_of_entry = function
    Vdecl(d) -> string_of_vdecl d
  | Vdef(d) -> string_of_vdef d
  | Stmt(s) -> string_of_stmt s
                                         
let string_of_fdef fdef =
  "func " ^ fdef.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdef.formals)
  ^ ") -> " ^ string_of_typ fdef.rtyp ^ ":{\n" ^
  String.concat "" (List.map string_of_entry fdef.body) ^ "}\n"

let string_of_bind = function
    Entry(e) -> string_of_entry e
  | Fdef(f) -> string_of_fdef f

let string_of_program bl = String.concat "" (List.map string_of_bind bl)
