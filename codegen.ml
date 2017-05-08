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

let translate (globals, functions, actors, structs) =
  let context = L.global_context () in
  let the_module = L.create_module context "Theatr"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context
  and d_t    = L.double_type context in

  (* list of tids of actors that have been instantiated. This is a reference - 
     not the functional way of doing it - but was easiest way to keep track of 
     actors that have been created *)
  let active_tids = ref [] in
  let max_actors = 1024 in

  (* create message struct of 
     { int switchCase; void *functionArgumentStruct ; void *sender }     *)
  let msg_struct_type = L.named_struct_type context "msg_struct" in
  let _ = L.struct_set_body msg_struct_type
                            [|i32_t; L.pointer_type i8_t ; L.pointer_type i8_t|]
  in

  (* actor_address_struct contains info if actor is alive, the thread's 
     tid (for joining), and its msgQueue 
     { int alive ; int tid ; void *msgQueue }               *)
  let actor_address_struct_type  = L.named_struct_type context "actor_address_struct" in
  let _ = L.struct_set_body actor_address_struct_type [| i32_t ; i32_t ; L.pointer_type i8_t |] false in
  let arr = Array.make max_actors (L.const_int i32_t 0) in
  let init = L.const_array actor_address_struct_type arr in 
  let global_actors = L.define_global "global_actors" init the_module in

  (* global to keep track of how many actors have been created so far 
     start the index at 1 because the main function gets the 0th position *)
  let init = L.const_int i32_t 1 in 
  let actor_count = L.define_global "actor_count" init the_module in


  let ltype_of_ptyp = function
      A.Int -> i32_t
    | A.Double -> d_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Actor -> (L.pointer_type i32_t)
    | A.String -> i32_t
  in

  let ltype_of_ctyp = function
      _ -> i32_t in
  
  let ltype_of_typ = function
      A.Ptyp p -> ltype_of_ptyp p
    | A.Ctyp (c, p) -> ltype_of_ctyp (c, p) in

  let handle_addition typ =
    match typ with
    | "double"    -> L.build_fadd
    | "i32"       -> L.build_add
    | _           -> raise (Failure("incompatible type passed to addition operation: " ^typ))
  in
   
  let handle_subtraction typ =
    match typ with
    | "double"    -> L.build_fsub
    | "i32"       -> L.build_sub
    | _           -> raise (Failure("incompatible type passed to subtraction operation: " ^typ))
  in
  
  let handle_mult typ = 
    match typ with
    | "double"    -> L.build_fmul
    | "i32"       -> L.build_mul
    | _           -> raise (Failure("incompatible type passed to multiplication operation: " ^typ))
  in
  
  let handle_div typ = 
    match typ with
    | "double"    -> L.build_fdiv
    | "i32"       -> L.build_sdiv
    | _           -> raise (Failure("incompatible type passed to division operation: " ^typ))
  in  
  
  let handle_arith_binop op typ1 typ2 =
    match typ1 with 
    | typ2  -> (
      match op with
      | A.Add   -> handle_addition typ1
      | A.Sub   -> handle_subtraction typ1
      | A.Mult  -> handle_mult typ1
      | A.Div   -> handle_div typ1
      | _       -> raise (Failure("incompatible type passed to arithmetic operation: " ^typ1))
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


  (* construct the struct types that will hold the arguments for each actor
     a pointer to an instance of these structs will be passed in during the pthread call *)
  let actor_struct_types =
    let actor_struct m adecl =
      let list_arg_types = List.map (fun (t,_) -> ltype_of_typ t) adecl.A.aformals in
      let type_array = Array.of_list list_arg_types in
      let name = adecl.A.aname in
      let struct_type = L.named_struct_type context (name ^ "_struct") in
      let _ = L.struct_set_body struct_type type_array false in
      StringMap.add name struct_type m in
    List.fold_left actor_struct StringMap.empty actors in

  (* construct the actor's main looping function to pass to pthread_create 
     This will be almost the same for each actor, the difference being the 
     specific local variables that each actor needs to maintain its state
     on its thread's stack *)
  let actor_decls =
    let actor_decl m adecl =
      let name = adecl.A.aname
      in let atype = L.function_type (L.pointer_type i8_t) [|L.pointer_type i8_t|] in
      (* type of the function is void function(void star), for pthread *)
      StringMap.add name (L.define_function name atype the_module, adecl) m in
    List.fold_left actor_decl StringMap.empty actors in
  
  let local_vars = ref StringMap.empty in
  let lookup n = try StringMap.find n !local_vars
                 with Not_found -> try StringMap.find n global_vars
                                   with Not_found -> raise (Failure ("undeclared variable " ^ n))
  in
  let rec expr builder = function
      A.IntLit i -> L.const_int i32_t i
    | A.DoubleLit f -> L.const_float d_t f
    | A.StringLit s ->
       let format_str_str s = L.build_global_stringptr (s^"\n") "fmt" builder in
       format_str_str s
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

       let act_func_name = match actuals with hd :: tl -> A.string_of_expr hd in
       let act_args = match actuals with hd :: tl -> tl in 
       let act_list_vals = List.map (expr builder) act_args in
       let act_list_types = List.map (L.type_of) act_list_vals in
       let act_vals_array = (Array.of_list act_list_vals) in
       let act_type_array = (Array.of_list act_list_types) in

       (* create struct type 
           set struct body to fill in the struct type 
           allocate an instance of the struct on the stack
           iterate through the args, store them in appropriate index on the stack
           cast the struct to a pointer type
        *)
       let act_struct_type = L.named_struct_type context (act_func_name ^ "_struct") in
       let _ = L.struct_set_body act_struct_type act_type_array false in 
       let act_struct = L.build_alloca act_struct_type "" builder in
       let index_and_store idx _ =
         let pt = L.build_struct_gep act_struct idx "" builder in
         let _ = L.build_store act_vals_array.(idx) pt builder in idx+1 in
       let _ = (match List.length act_list_vals with
                  0 -> -1
                | _ -> List.fold_left index_and_store 0 act_list_vals) in 
       let act_struct_casted = (match List.length act_list_vals with
                                  0 -> L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t)
                                | _ -> L.build_bitcast act_struct (L.pointer_type i8_t) "" builder) in

       let (fdef, _) = StringMap.find f function_decls in 
       let pthread_pt = L.build_alloca i32_t "tid" builder in  
       let attr = L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t) in
       let func = L.const_bitcast fdef (L.pointer_type (L.function_type (L.pointer_type i8_t) 
                                                                        [|L.pointer_type i8_t|])) in
       
       let args = [| pthread_pt ; attr ; func ; act_struct_casted |] in 
       let _ = L.build_call pthread_create_func args "pthread_create_result" builder in

       let join_attr = L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type (L.pointer_type i8_t)) in
       let pthread_pt_pid = L.build_load pthread_pt "tid" builder in

       L.build_call pthread_join_func [| pthread_pt_pid ; join_attr|] "pthread_join_result" builder

    | A.Call ("print", [e]) | A.Call ("printb", [e]) -> 
       let format_int_str = L.build_global_stringptr "%d\n" "fmt" builder
       and format_double_str = L.build_global_stringptr "%f\n" "fmt" builder in
       let format_str_str s = L.build_global_stringptr (s^"\n") "fmt" builder in

       let get_format_typ_str typ =
         match typ with
         | "i32" -> format_int_str
         | "double" -> format_double_str
         | _    -> raise (Failure("invalid type passed to print, "^typ))
       in
       let e1 = expr builder e in
       let typ_e = L.string_of_lltype (L.type_of e1) in
       if typ_e = "i8*" then
         L.build_call printf_func [| e1 |] "printf" builder
       else if typ_e = "i1" then
         L.build_call printf_func [| format_int_str ; (expr builder e) |] "printf" builder
       else
         let format_typ_str = get_format_typ_str typ_e in
         L.build_call printf_func [| format_typ_str; e1 |] "printf" builder
    | A.Call (f, act) ->
       let (fdef, fdecl) = StringMap.find f function_decls in
       let actuals = List.rev (List.map (expr builder) (List.rev act)) in
       let result = (match fdecl.A.typ with A.Ptyp(Void) -> ""
                                          | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list actuals) result builder
    (* codegen for actors: create a new struct on the stack to store arguments
         and pass a pointer to the struct, along with a pointer to the actor's
         function to pthread *)
    | A.NewActor (a, act) ->
       (* TODO: generate code to check if max_actors has been reached 
          and create a codepath to do something if no more actors allowed *)
       let (adef, adecl) = StringMap.find a actor_decls in
       let a_struct_type = StringMap.find a actor_struct_types in
       let actuals = List.rev (List.map (expr builder) (List.rev act)) in

       (* get position in global actors array *)
       let curr_actor_count = L.build_load actor_count "curr_actor_count" builder in
       let zero = L.const_int i32_t 0 in
       let addr_struct = L.build_in_bounds_gep global_actors
                         [| zero ; curr_actor_count |] "pos" builder in
       let alive = L.build_struct_gep addr_struct 0 "" builder in
       let _ = L.build_store (L.const_int i32_t 1) alive builder in
       let tid_in_struct = L.build_struct_gep addr_struct 1 "" builder in
       let msgQueue = L.build_struct_gep addr_struct 2 "" builder in
       (* TODO: do a build_store to put the msg queue pointer into msgQueue *)
       let result = addr_struct in
       
       (* increment actor count *)
       let new_actor_count = L.build_add curr_actor_count (L.const_int i32_t 1) "" builder in
       let _ = L.build_store new_actor_count actor_count builder in

       (* create the actor's function argument struct on the heap 
          this will be freed by the actor's pthread function after the args are 
          copied onto that thread's stack *)
       let a_struct = L.build_malloc a_struct_type "" builder in
       (* fill in the struct with actuals *)
       let index_and_store idx actual =
         let ptr = L.build_struct_gep a_struct idx "" builder in
         let _ = L.build_store actual ptr builder in
         idx+1 in
       let _ = (match List.length actuals with
                  0 -> -1
                | _ -> List.fold_left index_and_store 0 actuals) in
       let a_struct_ptr_casted = (match List.length actuals with
                                    0 -> L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t)
                                  | _ -> L.build_bitcast a_struct (L.pointer_type i8_t) "" builder) in
       let pthread_pt = L.build_alloca i32_t "tid" builder in
       let attr = L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t) in
       let pthread_args = [| pthread_pt ; attr ; adef ; a_struct_ptr_casted |] in
       let _ = L.build_call pthread_create_func pthread_args "pthread_create_result" builder in
       (* store the value of the tid into the actor_address_struct that you made before *)
       let tid_val = L.build_load pthread_pt "" builder in
       let _ = L.build_store tid_val tid_in_struct builder in
       L.build_bitcast result (L.pointer_type i32_t) "" builder
    | A.Send (msgFunction, msgArgs, recipientName) ->
       let result = L.const_int i32_t 42 (* TODO: fill out send code *) in
       result
                                
  in


  (* Fill in the body of the each actor's thread function *)
  let build_actor_thread_func_body adecl =
    let (the_function, _) = StringMap.find adecl.A.aname actor_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (* Construct the thread function's "locals": 
       Since this function is passed into pthread, it will have to 
       create a struct pointer that matches its arguments and dereference 
       the passed in ptr argument to fill out the argument variables on the stack
       ex:
       void *dolphin_actor(void *ptr) {
           struct dolphin_state *s = ptr;
           int height = s->height;
           int weight = s->weight;
       }
    *)
      (* create the struct pointer - steps copied from LLVM emitted by C code *)
    let voidp = Array.get (L.params the_function) 0 in
    let _ = L.set_value_name "ptr" voidp in
    let local_voidp = L.build_alloca (L.pointer_type i8_t) "" builder in
    let _ =  L.build_store voidp local_voidp builder in
    let struct_type = StringMap.find adecl.A.aname actor_struct_types in
    let local_struct_p = L.build_alloca (L.pointer_type struct_type) "state_struct" builder in
    let loaded_voidp = L.build_load local_voidp "" builder in
    let casted_voidp = L.build_bitcast loaded_voidp (L.pointer_type struct_type) "" builder in
    let _ = L.build_store casted_voidp local_struct_p builder in
    local_vars := StringMap.empty;

    (* create the formals as local variables, add them to locals map *)
    let add_formal (idx, struct_p) (t, n) = 
      let load_struct = L.build_load struct_p "" builder in
      (* need to cast 0 and idx to be i32_t before passing to index array in LLVM *)
      let zero = L.const_int i32_t 0 in 
      let idxVal = L.const_int i32_t idx in
      let var_at_idx = L.build_in_bounds_gep load_struct [|zero; idxVal|] "" builder in
      let var_stored = L.build_load var_at_idx "" builder in
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store var_stored local builder);
      local_vars := StringMap.add n local !local_vars; (idx+1, struct_p) in
    let add_local stmt = match stmt with
      | A.Vdecl (t, n) ->
         let local_var = L.build_alloca (ltype_of_typ t) n builder
         in local_vars := StringMap.add n local_var !local_vars
      | A.Vdef v ->
         let local_var = L.build_alloca (ltype_of_typ v.A.vtyp) v.A.vname builder
         in local_vars := StringMap.add v.A.vname local_var !local_vars
      | _ -> ()
    in
    let _ = List.fold_left add_formal (0, local_struct_p) adecl.A.aformals in
    List.iter add_local adecl.A.alocals;
    L.build_free voidp builder; (* free the malloc'd function arguments struct *)
  
    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Vdecl v -> ignore (v); builder (* we've already added this to locals *)
      | A.Vdef v -> ignore (expr builder v.A.vvalue); builder
      | A.Sdef s -> ignore (s); builder
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

    (* Execute actor's local statements *)
    let builder = stmt builder (A.Block adecl.A.alocals) in

    (***** Actor implementation 
     * Constant communication with message queue simulated through infite while loop.
     * Different message functionality simulated through switch-cases 
     * die() message exits infite while loop. 
     *
     * ******)
    let make_block name = 
        let bb = L.append_block context (name^"_bb") the_function in
        let builder = L.builder_at_end context bb in
        (bb, builder)
    in

    let build_pred_block pred bb builder body_bb merge_bb = 
        (*let bool_val = expr builder predicate in*)
        let bool_val = pred in

        (* Terminator for pred block *)
        L.position_at_end bb builder;
        ignore (L.build_cond_br bool_val body_bb merge_bb builder);
        builder
    in

    (* Returns list of msg blocks *)
    let build_msg_blocks = 
        let actor_local_vars_copy = ! local_vars in

        let create_msg_block decl = 
            let (msg_bb, msg_builder) = make_block ("msg_"^decl.A.mname^"_case") in
            (*TODO: set up formal vars and local vars of msg *)
            
            List.iter add_local decl.A.mbody;
            let msg_builder = stmt msg_builder (A.Block decl.A.mbody) in

            local_vars := actor_local_vars_copy;  (* Resets actor local vars *)
            L.position_at_end msg_bb msg_builder;
            msg_bb
        in
        let create_default_msg_block decl = 
            let (msg_bb, msg_builder) = make_block "msg_default_case" in
           
            List.iter add_local decl.A.dabody;
            let msg_builder = stmt msg_builder (A.Block decl.A.dabody) in
            local_vars := actor_local_vars_copy;  (* Resets actor local vars *)
             
            L.position_at_end msg_bb msg_builder;
            msg_bb
        in
        let create_die_msg_block = 
            let (die_bb, die_builder) = make_block "msg_die_case" in
            
            (* body of die msg *)

            L.position_at_end die_bb die_builder;  
            die_bb
        in
        
        let cases = List.map create_msg_block adecl.A.receives in
        let def_bb = create_default_msg_block adecl.A.drop in
        let die_bb = create_die_msg_block in

        let cases = def_bb :: die_bb :: cases in
        cases
    in

    let build_body_block bb builder finish_bb merge_bb =  

        (* pull message off queue and store the args struct pointer on the stack, 
           along with the message number*)
        
        (* TODO: here we add calls to dequeue the next msg from msgqueue *)
        
        let msgp = L.build_alloca i8_t "" builder  in (* TODO: replace this with actual msg pointer *)
        let _ = L.set_value_name "msg_ptr" msgp in
        let local_msgp = L.build_alloca (L.pointer_type i8_t) "local_msg_p" builder in
        let _ = L.build_store msgp local_msgp builder in
        let local_msg_struct_p = L.build_alloca (L.pointer_type msg_struct_type) "msg_struct_p" builder in
        let loaded_msgp = L.build_load local_msgp "" builder in
        let casted_msgp = L.build_bitcast loaded_msgp (L.pointer_type msg_struct_type) "" builder in
        ignore(L.build_store casted_msgp local_msg_struct_p builder);
        (* FOR SWITCH STATEMENTS: local_msg_struct_p is a pointer to the message struct *)

        let n = 20 in (* dummy msg num *)
        let msg_num = L.build_alloca i32_t "msg_case" builder in
        let _ = L.build_store (L.const_int i32_t n) msg_num builder in
        let num = L.build_load msg_num "" builder in 
        
        let cases = build_msg_blocks in

        (* Terminator for body block *)
        L.position_at_end bb builder;   
        let sw = L.build_switch num (List.hd cases) (List.length (List.tl cases)) builder in

        (*Adds cases to switch *)
        let add_msg_to_switch (count, li) bb = 
            let li = (count,bb) :: li in
            let case_num = L.const_int i32_t count in
            L.add_case sw case_num bb;
            (count+1, li)
        in
        let (_, c) = List.fold_left add_msg_to_switch (0, []) (List.tl cases) in 

        (* Add terminator to each msg case block 
         * 
         * COMMENT when testing msg body sequentially.
         * To test the body of just one msg, UNCOMMENT commedted line below.
         * It cases the specified case to branch to merge_bb instead of finish_bb *)
        let add_case_terminals (num, bb) = match num with 
            | 0 -> ignore(L.build_br merge_bb (L.builder_at_end context bb))
          (*| # -> ignore(L.build_br merge_bb (L.builder_at_end context bb)*)
            | _ -> ignore(L.build_br finish_bb (L.builder_at_end context bb))
        in
        let copy = c in
        let _ = List.map add_case_terminals copy in
        let def_bb = List.hd cases in
        ignore(L.build_br finish_bb (L.builder_at_end context def_bb));
       
        (* To test all msg bodies starting with default and moving to other cases in 
         * in rev sequential order, do following:
         * 1. UNCOMMENT to test all msg bodies sequentially 
         * 2. COMMENT prev section - Add terminator to cases 
         * 3. SET num to a high number than the  number of msg + 1 
         *    to ensure first case called is default case. 
         * 4. SET `pred` in build_actor_while to true, if not already 
         *    to ensure while loop running. 
         * *)
        (*let c = List.rev c in
        let connect_blocks (num, bb) = match num with 
          | 0 -> ignore(L.build_br merge_bb (L.builder_at_end context bb))
          | _ -> let branch_bb = snd (List.nth c (num - 1)) in 
                 ignore(L.build_br branch_bb (L.builder_at_end context bb))
        in
        let copy = List.rev c in
        let _ = List.map connect_blocks copy in
        let _ = ignore(L.build_br (snd (List.hd copy)) (L.builder_at_end context (List.hd cases))) in
        *)
        builder
    in
    
    let build_merge_block bb builder =  
        let ret_void_star = L.build_alloca (i8_t) "ret" builder in
        
        (* Terminator for merge block *)
        L.position_at_end bb builder;
        ignore(L.build_ret ret_void_star builder);
        builder
    in

    let build_actor_while =  
        let (pred_bb, pred_builder) = make_block "pred_msg_while" in
        let (body_bb, body_builder) = make_block "body_msg_while" in
        let (finish_bb, finish_builder) = make_block "finish_msg_while" in
        let (merge_bb, merge_builder) = make_block "merge_msg_while" in
        
        ignore (L.build_br pred_bb builder); (* Terminator for block calling while *) 
        ignore(L.build_br pred_bb finish_builder); (* Terminator for finish block *)
        
        (* Adds instructions and terminators for pred, body, and merge blocks *) 
        let pred = L.const_int i1_t 0 in (* if 1, while loop runs *)
        let _ = build_pred_block pred  pred_bb pred_builder body_bb merge_bb in
        let _ = build_body_block body_bb body_builder finish_bb merge_bb in
        let _ = build_merge_block merge_bb merge_builder in
        ()
    in
    build_actor_while in
  let _ = List.iter build_actor_thread_func_body actors in
  (* finished building actor thread functions, move on to normal functions *)

  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    local_vars := StringMap.empty;
    let add_formal (t, n) p = L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store p local builder);
    local_vars := StringMap.add n local !local_vars in
    List.iter2 add_formal fdecl.A.formals (Array.to_list (L.params the_function));
    (* make a pass through the function body for variable declarations 
         and add them to local_vars. This assumes that semantic checker 
         has gone through to make sure variables have been declared before 
         use *)
    let add_local stmt = match stmt with
	| A.Vdecl (t, n) ->
	    let local_var = L.build_alloca (ltype_of_typ t) n builder
	    in local_vars := StringMap.add n local_var !local_vars
    | A.Vdef v ->
	    let local_var = L.build_alloca (ltype_of_typ v.A.vtyp) v.A.vname builder
	    in local_vars := StringMap.add v.A.vname local_var !local_vars           
    | _ -> ()
    in
    List.iter add_local fdecl.A.body;

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
      | A.Return e ->
         (* if building the main() function, add pthread_joins before the return *)
         let builder = 
           if fdecl.A.fname = "main" then
             (* build a for loop through global array, running pthread join on the tids *)
             let i = L.build_alloca i32_t "" builder in
             local_vars := StringMap.add "return:i" i !local_vars;
             let e1 = A.Assign ("return:i", A.IntLit 1) in (* start index at 1 *)
             let e2 = A.Binop (A.Id "return:i", A.Less, A.IntLit max_actors) in
             let add_1_expr = A.Binop (A.Id "return:i", A.Add, A.IntLit 1) in
             let e3 = A.Assign ("return:i", add_1_expr) in

             let builder = stmt builder (A.Expr e1) in 
             let pred_bb = L.append_block context "while" the_function in
             ignore(L.build_br pred_bb builder);
             let body_bb = L.append_block context "while_body" the_function in

             let builder = L.builder_at_end context body_bb in
             let idx = L.build_load i "idx" builder in
             let zero = L.const_int i32_t 0 in
             let addr_struct = L.build_in_bounds_gep global_actors
                                    [| zero ; idx |] "pos" builder in
             let tid_p = L.build_struct_gep addr_struct 1 "tid_p" builder in
             let tid_p_cast = L.build_bitcast tid_p (L.pointer_type i32_t) "" builder in
             let tid_val = L.build_load tid_p_cast "tid_val" builder in
             let join_attr = L.const_bitcast (L.const_pointer_null i8_t)
                             (L.pointer_type (L.pointer_type i8_t)) in
             L.build_call pthread_join_func [| tid_val ; join_attr |] "pthread_join_result" builder;
             stmt builder (A.Expr e3);
             add_terminal builder (L.build_br pred_bb);
             let pred_builder = L.builder_at_end context pred_bb in
             let bool_val = expr pred_builder e2 in
             let merge_bb = L.append_block context "merge" the_function in
             ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
             L.builder_at_end context merge_bb
           else
             builder
         in
         ignore (match fdecl.A.typ with
	    A.Ptyp(Void) -> L.build_ret_void builder
	  | _ -> L.build_ret (expr builder e) builder); builder
      | A.Sdef s -> ignore (s); builder
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
                          | A.Ptyp(Void) -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter build_function_body functions;
  the_module
