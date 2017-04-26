(* Semantic checking for the Theatr compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions, actors) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
  
  (* Dynamically adds to MapString using list of tuples *)
  let rec add_to_map m li =
    match li with
    | [] -> m
    | hd :: tl -> add_to_map (StringMap.add (fst hd) (snd hd) m) tl
  in

  (**** Checking Actors ****)
  report_duplicate (fun n -> "duplicate actor " ^ n)
    (List.map (fun an -> an.aname) actors);

  let actor_decls = List.fold_left (fun m ad -> StringMap.add ad.aname ad m)
                                   StringMap.empty actors
  in 

  let actor_decl s = try StringMap.find s actor_decls
        with Not_found -> raise (Failure ("unrecognized actor " ^ s))
  in 

  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);
 
  (* Formats list of different print decls by type *)
  let format_print_decls typ = ("print:" ^ 
    string_of_typ typ,
    {typ = Void; fname = "print:" ^ string_of_typ typ; formals = [(typ, "x")]; body = []})
  in

  let print_typs = List.map format_print_decls [String; Int; Bool] in
  let built_in_decls = add_to_map StringMap.empty print_typs in
  
  (* Function declarations for named functions  *)
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                          built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (* Type of each variable (global, formal, or local 
       symbols is a reference, in order to be able to update the StringMap 
       as you iterate through each statement in the function, to check 
       that variables are declared before they are used
     *)
    let symbols = ref(List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals ))
    in

    let type_of_identifier s =
      try StringMap.find s !symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
    	IntLit _ -> Int
      | DoubleLit _ -> Double  
      | StringLit _ -> String
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
        | Add | Sub | Mult | Div when t1 = Double && t2 = Double -> Double
	| Equal | Neq when t1 = t2 -> Bool
	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign (type_of_identifier var) (expr e)
            (Failure ("illegal assignment " ^ string_of_typ lt ^ 
            " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
      | Call("pthread_create", actuals) ->
        if List.length actuals > 0 then
            let fname = List.hd actuals in
            let new_call = Call(string_of_expr fname, List.tl actuals) in
            expr new_call
        else 
            raise(Failure("expecting at least one argument in pthread_create(): fname, func_arguments"))

      | Call("print", actuals) as call ->
        if List.length actuals == 1 then
            let et = expr (List.hd actuals) in
            let fd = function_decl ("print:" ^ string_of_typ et) in
            fd.typ
        else
            raise (Failure ("expecting 1 arguments in " ^ 
                string_of_expr call))

      | Call(fname, actuals) as call -> let fd = function_decl fname in
        if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int
            (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
        else
            List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ 
                string_of_typ et ^ " expected " ^ string_of_typ ft ^ 
                " in " ^ string_of_expr e))))
              fd.formals actuals;
            fd.typ

      (* check actor instantiation with constructor signature of that actor *)
      | NewActor(aname, actuals) as ex -> let ad = actor_decl aname in
        if List.length actuals != List.length ad.aformals then 
          raise (Failure ("expecting " ^ string_of_int (List.length ad.aformals)
          ^ " arguments in " ^ string_of_expr ex))
        else
            List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ 
                string_of_typ et ^ " expected " ^ string_of_typ ft ^ 
                " in " ^ string_of_expr e))))
              ad.aformals actuals;
            Actor


    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      (* Vdecl updates symbols via reference to add a variable declaration *)
      | Vdecl (t, n) ->
	 if (StringMap.mem n !symbols) then raise (Failure ("local variable " ^ n ^ " already exists"));
	 symbols := StringMap.add n t !symbols;
	 check_not_void (fun n -> "illegal void local: " ^ n) (t, n);
	 ignore((t, n))
      | Vdef vdef ->
	 if (StringMap.mem vdef.vname !symbols) then raise (Failure ("local variable " ^ vdef.vname ^ " already exists"));
         symbols := StringMap.add vdef.vname vdef.vtyp !symbols;
	 check_not_void (fun n -> "illegal void local: " ^ n) (vdef.vtyp, vdef.vname);
         ignore(vdef)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions
