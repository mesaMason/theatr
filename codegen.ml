(* codegen in progress *)
(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
LLVM tutorial: Make sure to read the OCaml version of the tutorial
http://llvm.org/docs/tutorial/index.html
Detailed documentation on the OCaml LLVM library:
http://llvm.moe/
http://llvm.moe/ocaml/
*)
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions, actors) =
  let context = L.global_context () in
  let the_module = L.create_module context "Theatr"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context
  and d_t    = L.double_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Double -> d_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Actor -> i32_t in (* TODO: change this to be a pointer *)

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare pthread_create() and pthread_join(), which will be called to spawn actors *)
  let pthread_t = i32_t 
  and func_void_ptr_void_ptr = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] 
  in

  let pthread_create_t_arr = [|L.pointer_type i32_t ; L.pointer_type i8_t; 
    L.pointer_type func_void_ptr_void_ptr ; L.pointer_type i8_t |]
  in
  let pthread_create_t = L.function_type i32_t pthread_create_t_arr in
  let pthread_create_func = L.declare_function "pthread_create" pthread_create_t the_module in

  let pthread_join_arr = [| pthread_t ; L.pointer_type (L.pointer_type i8_t) |] in
  let pthread_join_t = L.function_type i32_t pthread_join_arr in
  let pthread_join_func = L.declare_function "pthread_join" pthread_join_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    let format_int_str = L.build_global_stringptr "%d\n" "fmt" builder
    and format_double_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let format_str_str s = L.build_global_stringptr (s^"\n") "fmt" builder in

    let get_format_typ_str typ =
        match typ with
      | "i32" -> format_int_str
      | "double" -> format_double_str
      | _    -> raise (Failure("invalid type passed to print, "^typ))
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

      (* make a pass through the function body for variable declarations 
         and add them to local_vars. This assumes that semantic checker 
         has gone through to make sure variables have been declared before 
         use *)
      let add_local m stmt = match stmt with
	| A.Vdecl (t, n) ->
	    let local_var = L.build_alloca (ltype_of_typ t) n builder
	    in StringMap.add n local_var m
        | A.Vdef v ->
	    let local_var = L.build_alloca (ltype_of_typ v.A.vtyp) v.A.vname builder
	    in StringMap.add v.A.vname local_var m           
        | _ -> m
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.body in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                 with Not_found -> try StringMap.find n global_vars
                 with Not_found -> raise (Failure ("undeclared variable " ^ n))
    in
    
    let handle_addition typ =
        match typ with
      | "double"    -> L.build_fadd
      | "i32"       -> L.build_add
    in
   
    let handle_subtraction typ =
        match typ with
      | "double"    -> L.build_fsub
      | "i32"       -> L.build_sub
    in

    let handle_mult typ = 
        match typ with
      | "double"    -> L.build_fmul
      | "i32"       -> L.build_mul
    in
    
    let handle_div typ = 
        match typ with
      | "double"    -> L.build_fdiv
      | "i32"       -> L.build_sdiv
    in  

    let handle_arith_binop op typ1 typ2 =
        match typ1 with 
        | typ2  -> (
            match op with
          | A.Add   -> handle_addition typ1
          | A.Sub   -> handle_subtraction typ1
          | A.Mult  -> handle_mult typ1
          | A.Div   -> handle_div typ1
          )
        | _     -> raise (Failure("incompatible types passed to arithmetic operation: " ^typ1 ^typ2))    
    in
   
    let handle_equal typ =
        match typ with 
      | "double"    -> L.build_fcmp L.Fcmp.Oeq
      | "i32"       -> L.build_icmp L.Icmp.Eq
    in
    let handle_neq typ =
        match typ with 
      | "double"    -> L.build_fcmp L.Fcmp.One
      | "i32"    -> L.build_icmp L.Icmp.Ne
    in
    let handle_less typ =
        match typ with
      | "double"    -> L.build_fcmp L.Fcmp.Olt
      | "i32"    -> L.build_icmp L.Icmp.Slt
    in
    let handle_leq typ =
        match typ with
      | "double"    -> L.build_fcmp L.Fcmp.Ole
      | "i32"    -> L.build_icmp L.Icmp.Sle
    in
    let handle_greater typ =
        match typ with
      | "double"    -> L.build_fcmp L.Fcmp.Ogt
      | "i32"    -> L.build_icmp L.Icmp.Sgt
    in
    let handle_geq typ =
        match typ with
      | "double"    -> L.build_fcmp L.Fcmp.Oge
      | "i32"    -> L.build_icmp L.Icmp.Sge
    in
    let handle_comp_binop op typ1 typ2 =
        match typ1 with
        | typ2 -> (
            match op with
          | A.Equal     -> handle_equal typ1
          | A.Neq       -> handle_neq typ1 
          | A.Less      -> handle_less typ1
          | A.Leq       -> handle_leq typ1
          | A.Greater   -> handle_greater typ1
          | A.Geq       -> handle_geq typ1
          )
        | _             -> raise (Failure("incompatible types passed to comparison operation: " ^typ1 ^typ2))
    in
      
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.DoubleLit f -> L.const_float d_t f
      | A.StringLit s -> format_str_str s
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in

        let typ_e1 = L.string_of_lltype(L.type_of e1')
        and typ_e2 = L.string_of_lltype(L.type_of e2') in
          (match op with
            | A.Add | A.Sub | A.Mult | A.Div   -> handle_arith_binop op typ_e1 typ_e2
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal | A.Neq | A.Less | A.Leq | A.Greater | A.Geq  -> handle_comp_binop op typ_e1 typ_e2
          ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
      let e' = expr builder e in
      (match op with
          A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call("pthread_create", actuals) ->
        let f = (match (List.hd actuals) with 
                    A.Id s -> s
                  | _ -> raise(Failure ("expected valid func id for pthread()"))) in 
        
        let act = (match List.length actuals with
                    1 -> L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t) 
                  | 2 -> L.const_inttoptr (expr builder (List.nth actuals 1)) (L.pointer_type i8_t) 
                  | _ -> raise(Failure ("invalid number of arguments for pthread_create()"))) in 
        
        let (fdef, _) = StringMap.find f function_decls in 
        let pthread_pt = L.build_alloca i32_t "tid" builder in  
        let attr = L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t) in
        let func = L.const_bitcast fdef (L.pointer_type (L.function_type (L.pointer_type i8_t) 
                    [|L.pointer_type i8_t|])) in
        
        let args = [| pthread_pt ; attr ; func ; act |] in 
        let _ = L.build_call pthread_create_func args "pthread_create_result" builder in

        let join_attr = L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type (L.pointer_type i8_t)) in
        let pthread_pt_pid = L.build_load pthread_pt "tid" builder in

        L.build_call pthread_join_func [| pthread_pt_pid ; join_attr|] "pthread_join_result" builder

      | A.Call ("print", [e]) -> 
        let e1 = expr builder e in
        let typ_e = L.string_of_lltype (L.type_of e1) in

        if typ_e = "i8*" then
            L.build_call printf_func [| e1 |] "printf" builder
        else if typ_e = "i1" then
            if e1 = (L.const_int i1_t 1) then 
                let eb = format_str_str "true" in
                L.build_call printf_func [| eb |] "printf" builder
            else 
                let eb = format_str_str "false" in
                L.build_call printf_func [| eb |] "printf" builder
        else
            let format_typ_str = get_format_typ_str typ_e in
            L.build_call printf_func [| format_typ_str; e1 |] "printf" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	     let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	     let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
      (* TODO: codegen for actors *)
      | A.NewActor (a, act) -> L.const_int i32_t 42
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
  A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Vdecl v -> ignore (v); builder (* we've already added this to locals *)
      | A.Vdef v -> ignore (expr builder v.A.vvalue); builder
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
  | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
   let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
     (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
     (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in
    
    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  
  List.iter build_function_body functions;
  the_module
