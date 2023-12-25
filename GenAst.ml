open Llvm
open Llvm_analysis
open Llvm_scalar_opts
open Llvm_ipo
open Llvm_vectorize
open Llvm_target
open Llvm.PassManager
open Llvm_passmgr_builder
open Ast
open Symbol
open Types

exception Error of string

let context = global_context ()
let the_module = create_module context "my_module"
let builder = builder context
let int_type = i64_type context
let char_type = i8_type context
let bool_type = i1_type context

let lib_function_names =
  [
    "writeInteger";
    "writeChar";
    "writeString";
    "readInteger";
    "readChar";
    "readString";
    "ascii";
    "chr";
    "strlen";
    "strcmp";
    "strcpy";
    "strcat";
  ]

let build_nop () =
  let zero = const_int bool_type 0 in
  build_add zero zero "nop" builder

(* Symbol table that holds the memory location of the variable in question*)
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 2000
let named_functions = Hashtbl.create 2000
let blocks_list = ref []

let rec lltype_of_t_type x =
  match x with
  | T_int -> int_type
  | T_char -> char_type
  | T_array (t, n) -> array_type (lltype_of_t_type t) n
  | T_func t -> lltype_of_t_type t
  | T_none -> void_type context

and t_type_of_lltype lltype =
  match Llvm.classify_type lltype with
  | TypeKind.Integer -> begin
      match Llvm.integer_bitwidth lltype with
      | 64 -> T_int
      | 8 -> T_char
      | _ -> assert false
    end
  | TypeKind.Array ->
      T_array
        (t_type_of_lltype (Llvm.element_type lltype), Llvm.array_length lltype)
  | TypeKind.Function -> T_func (t_type_of_lltype (return_type lltype))
  | TypeKind.Void -> T_none
  | _ -> raise (Invalid_argument "t_type is invalid")

and lltype_of_fparDef x =
  let t_type = Ast.t_type_of_dataType x.fpar_type.data_type in
  match x.ref with
  | false -> lltype_of_t_type t_type
  | true ->
      (* TODO: need to add a check here whether type is
          array. Maybe need to change fparType and add type array there *)
      pointer_type (lltype_of_t_type t_type)

(* [ {ref, [a, b], int}, {noref, [c,d], char} ] -->
   [ {ref, [a], int}, {ref, [b], int}, {noref, [c], char}, {noref, [d], char} ] *)
let rec expand_fpar_def_list (def_list : fparDef list) : fparDef list =
  let expand_fpar_def def =
    List.map
      (fun id -> { ref = def.ref; id_list = [ id ]; fpar_type = def.fpar_type })
      def.id_list
  in
  List.concat (List.map expand_fpar_def def_list)

and gen_lvalue_address id (stack_frame_alloca : Llvm.llvalue) funcDef
    stack_frame_length =
  let rec iterate i stack_frame funcDef =
    let var_name, elem_pos, is_ref = List.nth funcDef.var_records i in
    if var_name = id then begin
      let addr = build_struct_gep stack_frame elem_pos var_name builder in
      if is_ref = false then
        addr
      else
        build_load addr (var_name ^ "_address") builder
    end
    else if i + 1 < funcDef.stack_frame_length then
      iterate (i + 1) stack_frame funcDef
    else
      match funcDef.parent_func with
      | Some p ->
          let access_link =
            build_struct_gep stack_frame 0 "access_link_ptr" builder
          in
          let the_access_link =
            build_load access_link "access_link_val" builder
          in
          iterate 0 the_access_link p
      | None -> failwith "fail variable not found"
  in
  iterate 0 stack_frame_alloca funcDef

and gen_expr is_param_ref (stack_frame_alloca : Llvm.llvalue) stack_frame_length
    funcDef expr =
  match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_int char_type (int_of_char x)
  | E_lvalue lv ->
      (* [gen_lvalue_kind ?poc lv] takes an l-value [lv] and returns a pointer
         pointing at the [Llvm.llvalue] of the l-value. The optional argument
         [poc] shouldn't concern the top level use of the function.
         Returns [Llvm.llvalue]. *)
      let rec gen_lvalue_kind ?(partOfComp = false) = function
        | L_id id ->
            let lv_address =
              gen_lvalue_address id stack_frame_alloca funcDef
                stack_frame_length
            in
            if not partOfComp then
              if is_param_ref = false then
                build_load lv_address id builder
              else
                lv_address
            else (* failwith "TODO: gen_expr (E_lvalue (L_comp (L_id _)))" *)
              build_gep lv_address
                [| const_int int_type 0 |]
                "array_pointer" builder
        | L_string s ->
            (* pointer_type (const_string context s) *)
            (* create const string *)
            let const_str = const_stringz context s in
            (* create variable holding  const_str*)
            let string_var = define_global "string_var" const_str the_module in
            (* point to the first element (char) of the string *)
            let zero = const_int int_type 0 in
            build_gep string_var [| zero; zero |] "string_ptr" builder
        | L_comp (lv, e) -> begin
            let elementPtr =
              let arrayPtr = gen_lvalue_kind ~partOfComp:true lv in
              let index =
                let indexExpr =
                  gen_expr false stack_frame_alloca stack_frame_length funcDef e
                in
                build_intcast indexExpr int_type "index_cast" builder
              in
              build_gep arrayPtr [| index |] "array_element_ptr" builder
            in
            (* TODO: The if-else stmt below might be unnecessary/wrong. *)
            if not partOfComp then
              build_load elementPtr "array_elem" builder
            else
              failwith "TODO: gen_expr (E_lvalue (L_comp (L_comp _)))"
          end
      in
      gen_lvalue_kind lv.lv_kind
  | E_func_call fc ->
      let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
      let callee = fc.id in
      let args_list = fc.expr_list in
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      ignore (params callee);
      (* if 'params' has no side effects, delete line*)
      let i = ref 0 in
      let res = ref [] in
      List.iter
        (fun x ->
          let ith_elem = List.nth args_list !i in
          res :=
            gen_expr x.ref stack_frame_alloca stack_frame_length funcDef
              ith_elem
            :: !res;
          incr i)
        fpar_def_list;
      let rev_list = List.rev !res in
      let args_array =
        let first_argument =
          if fc.id = funcDef.header.id then begin
            let access_link_ptr =
              build_struct_gep stack_frame_alloca 0 "access_link_ptr" builder
            in
            let access_link_val =
              build_load access_link_ptr "access_link_ptr" builder
            in
            access_link_val
          end
          else
            stack_frame_alloca
        in
        if List.mem fc.id lib_function_names = false then
          Array.of_list ([ first_argument ] @ rev_list)
        else
          Array.of_list rev_list
      in
      build_call callee args_array "" builder
  | E_sgn_expr (sign, expr) -> (
      match sign with
      | O_plus ->
          gen_expr false stack_frame_alloca stack_frame_length funcDef expr
      | O_minus ->
          build_neg
            (gen_expr false stack_frame_alloca stack_frame_length funcDef expr)
            "minus" builder)
  | E_op_expr_expr (lhs, oper, rhs) -> (
      let lhs_val =
        gen_expr false stack_frame_alloca stack_frame_length funcDef lhs
      in
      let rhs_val =
        gen_expr false stack_frame_alloca stack_frame_length funcDef rhs
      in
      match oper with
      | O_plus -> build_add lhs_val rhs_val "addtmp" builder
      | O_minus -> build_sub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_val "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_val "modtmp" builder)
  | E_expr_parenthesized expr ->
      gen_expr false stack_frame_alloca stack_frame_length funcDef expr

and gen_cond (stack_frame_alloca : Llvm.llvalue) stack_frame_length funcDef =
  function
  | C_not_cond (lo, c) ->
      let ll_cond = gen_cond stack_frame_alloca stack_frame_length funcDef c in
      build_not ll_cond "not" builder
  | C_cond_cond (c1, lo, c2) -> (
      (* TODO: short-circuiting *)
      let gen_cond = gen_cond stack_frame_alloca stack_frame_length funcDef in
      let lhs_val = gen_cond c1 in
      let rhs_val = gen_cond c2 in
      match lo with
      | O_and -> build_and lhs_val rhs_val "and" builder
      | O_or -> build_or lhs_val rhs_val "or" builder
      | O_not -> assert false)
  | C_expr_expr (e1, co, e2) -> (
      let gen_expr =
        gen_expr false stack_frame_alloca stack_frame_length funcDef
      in
      let lhs_val = gen_expr e1 in
      let rhs_val = gen_expr e2 in
      let open Icmp in
      let build_comp predicate instr_name =
        build_icmp predicate lhs_val rhs_val instr_name builder
      in
      match co with
      | O_equal -> build_comp Eq "equal"
      | O_less -> build_comp Slt "less"
      | O_greater -> build_comp Sgt "greater"
      | O_less_eq -> build_comp Sle "less_eq"
      | O_greater_eq -> build_comp Sge "greater_eq"
      | O_not_equal -> build_comp Ne "not_equal")
  | C_cond_parenthesized c ->
      gen_cond stack_frame_alloca stack_frame_length funcDef c

and gen_stmt (stack_frame_alloca : Llvm.llvalue) stack_frame_length funcDef stmt
    =
  (* [extract_ret_stmt (s : Ast.stmt)] calculates whether the statment [s] always
     includes a return statement, and if it is, a ret statement is generated in
     the LLVM IR code so that a type error is caught. If the statement [s]
     doesn't return a value, a default value [dv] is returned. Returns
     [Llvm.llvalue]. *)
  let extract_ret_stmt s =
    (* [type_of_stmt (s : Ast.stmt)] takes a statement [s] and returns a
       [Types.t_type option]. If the statement [s] is a return statement or a
       statement that always includes a return statement, then [Some t] is
       returned, with [t] being the type of the expression returned, else [None].
       Returns [Types.t_type option]. *)
    let rec type_of_stmt : Ast.stmt -> Types.t_type option = function
      | S_return e_opt ->
          (* [type_of_expr (e : Ast.expr)] takes an expression [e] and returns
             its type. Returns [Types.t_type]. *)
          let rec type_of_expr = function
            | E_const_int _ | E_sgn_expr _ | E_op_expr_expr _ -> T_int
            | E_const_char _ -> T_char
            | E_lvalue lv -> begin
                try Option.get lv.lv_type
                with Invalid_argument _ ->
                  failwith "Type of l-value should be already set."
              end
            | E_func_call fc -> begin
                try Option.get fc.ret_type
                with Invalid_argument _ ->
                  failwith "Type of function call should be already set."
              end
            | E_expr_parenthesized e -> type_of_expr e
          in
          Some (match e_opt with None -> T_none | Some e -> type_of_expr e)
      | S_block (Block l) ->
          let rec type_of_stmt_list = function
            | [] -> None
            | h :: t ->
                let typeOfHead = type_of_stmt h in
                if typeOfHead <> None then
                  typeOfHead
                else
                  type_of_stmt_list t
          in
          type_of_stmt_list l
      | S_if_else (c, s1, s2) -> begin
          match get_const_cond_value c with
          | Some b -> type_of_stmt (if b then s1 else s2)
          | None ->
              let typeOfS1 = type_of_stmt s1 in
              let typeOfS2 = type_of_stmt s2 in
              if typeOfS1 = None || typeOfS2 = None then
                None
              else
                typeOfS1
        end
      | S_while (c, s) | S_if (c, s) -> begin
          match get_const_cond_value c with
          | None | Some false -> None
          | Some true -> type_of_stmt s
        end
      | S_assignment _ | S_func_call _ | S_semicolon -> None
    in
    match type_of_stmt s with
    | None -> build_nop ()
    | Some t -> begin
        match t with
        | T_int -> build_ret (const_int int_type 0) builder
        | T_char -> build_ret (const_int char_type 0) builder
        | T_none -> build_nop ()
        | T_array _ | T_func _ -> assert false
      end
  in
  match stmt with
  | S_assignment (lv, expr) -> begin
      match lv.lv_kind with
      | L_id id ->
          let lv_address =
            gen_lvalue_address id stack_frame_alloca funcDef stack_frame_length
          in
          let lv_value =
            gen_expr false stack_frame_alloca stack_frame_length funcDef expr
          in
          build_store lv_value lv_address builder
      | L_string _ -> assert false
      | L_comp (lv, e) ->
          let index =
            let indexExpr =
              gen_expr false stack_frame_alloca stack_frame_length funcDef e
            in
            build_intcast indexExpr int_type "index_cast" builder
          in
          let gen_lvalue = function
            | L_string _ -> assert false
            | L_id id ->
                let lv_address =
                  gen_lvalue_address id stack_frame_alloca funcDef
                    stack_frame_length
                in
                build_struct_gep lv_address 0 "array_pointer" builder
            | L_comp _ -> failwith "TODO: gen_stmt: gen_lvalue (L_comp _)"
          in
          let elementPtr =
            let arrayPtr = gen_lvalue lv in
            build_gep arrayPtr [| index |] "array_element_ptr" builder
          in
          let value =
            gen_expr false stack_frame_alloca stack_frame_length funcDef expr
          in
          build_store value elementPtr builder
    end
  | S_func_call fc ->
      let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
      let callee = fc.id in
      let args_list = fc.expr_list in
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      ignore (params callee);
      (* if 'params' has no side effects, delete line*)
      let i = ref 0 in
      let res = ref [] in

      List.iter
        (fun fpar_def ->
          let ith_elem = List.nth args_list !i in
          res :=
            gen_expr fpar_def.ref stack_frame_alloca stack_frame_length funcDef
              ith_elem
            :: !res;
          incr i)
        fpar_def_list;
      let rev_list = List.rev !res in
      let args_array =
        let first_argument =
          if fc.id = funcDef.header.id then begin
            let access_link_ptr =
              build_struct_gep stack_frame_alloca 0 "access_link_ptr" builder
            in
            let access_link_val =
              build_load access_link_ptr "access_link_ptr" builder
            in
            access_link_val
          end
          else
            stack_frame_alloca
        in
        if List.mem fc.id lib_function_names = false then
          Array.of_list ([ first_argument ] @ rev_list)
        else
          Array.of_list rev_list
      in
      build_call callee args_array "" builder
  | S_block b -> begin
      match b with
      | Block [] -> build_nop ()
      | Block l ->
          let get_last_elem = function
            | [] -> assert false
            | l -> List.(hd (rev l))
          in
          get_last_elem
            (List.map
               (gen_stmt stack_frame_alloca stack_frame_length funcDef)
               l)
    end
  | S_if (c, s) ->
      let start_basic_block = insertion_block builder in
      let function_bb = block_parent start_basic_block in
      let then_basic_block = append_block context "then" function_bb in
      let merge_basic_block = append_block context "if_then_cont" function_bb in

      position_at_end start_basic_block builder;
      let cond_val () =
        let zero = const_int bool_type 0 in
        let ll_c = gen_cond stack_frame_alloca stack_frame_length funcDef c in
        build_icmp Ne zero ll_c "if_then_cond" builder
      in
      ignore
        (build_cond_br (cond_val ()) then_basic_block merge_basic_block builder);

      position_at_end then_basic_block builder;
      ignore (gen_stmt stack_frame_alloca stack_frame_length funcDef s);
      let new_then_basic_block = insertion_block builder in

      position_at_end new_then_basic_block builder;
      ignore (build_br merge_basic_block builder);
      position_at_end merge_basic_block builder;
      extract_ret_stmt stmt
  | S_if_else (c, s1, s2) ->
      let start_basic_block = insertion_block builder in
      let function_bb = block_parent start_basic_block in
      let then_basic_block = append_block context "then" function_bb in
      let else_basic_block = append_block context "else" function_bb in
      let merge_basic_block =
        append_block context "if_then_else_cont" function_bb
      in

      let cond_val () =
        let zero = const_int bool_type 0 in
        let ll_c = gen_cond stack_frame_alloca stack_frame_length funcDef c in
        build_icmp Ne zero ll_c "if_then_else_cond" builder
      in
      position_at_end start_basic_block builder;
      ignore
        (build_cond_br (cond_val ()) then_basic_block else_basic_block builder);

      let gen_stmt = gen_stmt stack_frame_alloca stack_frame_length funcDef in
      position_at_end then_basic_block builder;
      ignore (gen_stmt s1);
      let new_then_basic_block = insertion_block builder in
      position_at_end new_then_basic_block builder;
      ignore (build_br merge_basic_block builder);

      position_at_end else_basic_block builder;
      ignore (gen_stmt s2);
      let new_else_basic_block = insertion_block builder in
      position_at_end new_else_basic_block builder;
      ignore (build_br merge_basic_block builder);

      position_at_end merge_basic_block builder;
      extract_ret_stmt stmt
  | S_while (c, s) ->
      let start_basic_block = insertion_block builder in
      let function_bb = block_parent start_basic_block in
      let while_basic_block = append_block context "while" function_bb in
      let cont_basic_block = append_block context "while_cont" function_bb in

      let cond_val () =
        let zero = const_int bool_type 0 in
        let ll_c = gen_cond stack_frame_alloca stack_frame_length funcDef c in
        build_icmp Ne zero ll_c "while_cond" builder
      in
      position_at_end start_basic_block builder;
      ignore
        (build_cond_br (cond_val ()) while_basic_block cont_basic_block builder);
      position_at_end while_basic_block builder;
      ignore (gen_stmt stack_frame_alloca stack_frame_length funcDef s);

      let new_while_basic_block = insertion_block builder in
      position_at_end new_while_basic_block builder;
      ignore
        (build_cond_br (cond_val ()) while_basic_block cont_basic_block builder);

      position_at_end cont_basic_block builder;
      extract_ret_stmt stmt
  | S_return expr_opt -> (
      match expr_opt with
      | None -> build_ret_void builder
      | Some e ->
          let ll_expr =
            gen_expr false stack_frame_alloca stack_frame_length funcDef e
          in
          build_ret ll_expr builder)
  | S_semicolon -> build_nop ()

and gen_header (header : Ast.header) (access_link : Llvm.lltype option) =
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  let ret_type = header.ret_type in
  let access_link_list =
    match access_link with Some x -> [ x ] | None -> []
  in

  let param_types_list = access_link_list @ List.map lltype_of_fparDef args in
  let param_types_array = Array.of_list param_types_list in
  let return_type = lltype_of_t_type (Ast.t_type_of_retType ret_type) in
  let ft = function_type return_type param_types_array in
  let f =
    match lookup_function name the_module with
    | None -> (
        (* if access_link is None then function is main, so give the name name to llvm for global function *)
        match access_link with
        | Some _ -> declare_function name ft the_module
        | None -> declare_function "main" ft the_module)
    | Some x -> failwith "semantic analysis error: function already defined"
  in
  f

and gen_funcDef funcDef =
  let funcDef_ll = gen_header funcDef.header funcDef.access_link in
  let bb = append_block context ("entry_" ^ funcDef.header.id) funcDef_ll in
  position_at_end bb builder;
  blocks_list := bb :: !blocks_list;
  let params_records = ref [] in
  let stack_frame_type =
    match funcDef.stack_frame with
    | Some sf -> sf
    | None -> failwith "Fail. Stack frame for function not set."
  in
  let frame_array = struct_element_types stack_frame_type in
  let stack_frame_length = Array.length frame_array in
  funcDef.stack_frame_length <- stack_frame_length;
  let access_link_list =
    match funcDef.access_link with Some al -> [ al ] | None -> []
  in
  let stack_frame_alloca =
    let stack_frame_name =
      match access_link_list with [] -> "main" | list -> funcDef.header.id
    in
    build_alloca stack_frame_type ("stack_frame_" ^ stack_frame_name) builder
  in
  let args = expand_fpar_def_list funcDef.header.fpar_def_list in
  let args_array = Array.of_list args in

  Array.iteri
    (fun i param ->
      let position =
        build_struct_gep stack_frame_alloca i "stack_frame elem" builder
      in
      (* i = 0 corresponds to the access link*)
      if i = 0 then (
        match access_link_list with
        (* funcion is main *)
        | [] ->
            let ith_param = args_array.(i) in
            let var_name =
              try List.hd ith_param.id_list with _ -> assert false
            in
            set_value_name var_name position;
            params_records := (var_name, i, ith_param.ref) :: !params_records;
            ignore (build_store param position builder)
        | list ->
            set_value_name "access_link" position;
            params_records := ("access_link", i, false) :: !params_records;
            ignore (build_store param position builder))
      else
        let ith_param =
          match access_link_list with
          | [] -> args_array.(i)
          | list -> args_array.(i - 1)
        in
        let var_name = try List.hd ith_param.id_list with _ -> assert false in
        set_value_name var_name position;
        params_records := (var_name, i, ith_param.ref) :: !params_records;
        ignore (build_store param position builder))
    (params funcDef_ll);
  params_records := List.rev !params_records;
  funcDef.var_records <- !params_records @ funcDef.var_records;
  funcDef.stack_frame_addr <- Some stack_frame_alloca;

  (* iterate functions in dfs order *)
  let rec iterate local_def =
    let struct_index = ref (Array.length (params funcDef_ll)) in
    match local_def with
    | L_varDef v ->
        List.iter
          (fun id ->
            let position =
              build_struct_gep stack_frame_alloca !struct_index id builder
            in
            set_value_name id position;
            incr struct_index)
          v.id_list
    | L_funcDef fd ->
        if Types.debugMode then
          Printf.printf "func in iterate %s\n%!" fd.header.id;
        gen_funcDef fd
    | L_funcDecl fdl -> failwith "TODO gen_funcDef: iterate (L_funcDecl _)"
  in
  List.iter iterate funcDef.local_def_list;

  let stmt_list = match funcDef.block with Block b -> b in
  ignore
    (List.map
       (gen_stmt stack_frame_alloca stack_frame_length funcDef)
       stmt_list);

  if block_terminator @@ insertion_block builder = None then
    ignore (build_ret_void builder);
  blocks_list :=
    List.tl !blocks_list (* if blocks_list = [] this will throw Failure _ *);
  if !blocks_list <> [] then
    position_at_end (List.hd !blocks_list) builder

let define_lib_funcs () =
  let define_lib_func
      ((name : string), (args : Ast.fparDef list), (ret_type : Ast.retType)) =
    let args = expand_fpar_def_list args in
    let args_array = Array.of_list args in
    Hashtbl.add named_functions (Hashtbl.hash name) args;
    let param_types_list = List.map lltype_of_fparDef args in
    let param_types_array = Array.of_list param_types_list in
    let return_type = lltype_of_t_type (Ast.t_type_of_retType ret_type) in
    let ft = function_type return_type param_types_array in
    let f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some x -> failwith "semantic analysis error: function already defined"
    in
    (* Set names for all arguments. *)
    Array.iteri
      (fun i param ->
        let n = try List.hd args_array.(i).id_list with _ -> assert false in
        (* Set the name of each argument which is an llvalue, to a string *)
        set_value_name n param;
        Hashtbl.add named_values n param)
      (params f)
  in

  (* writeInteger (n : int) : nothing *)
  let writeInteger_fp_type = newFparType (ConstInt, []) in
  let writeInteger_fp_def_list =
    [ newFparDef (false, [ "x" ], writeInteger_fp_type) ]
  and writeInteger_f_rtype = Nothing in

  (* writeChar (c : char) : nothing *)
  let writeChar_fp_type = newFparType (ConstChar, []) in
  let writeChar_fp_def_list = [ newFparDef (false, [ "x" ], writeChar_fp_type) ]
  and writeChar_f_rtype = Nothing in

  (* writeString (s char[]) : nothing *)
  let writeString_fp_def_list =
    [ newFparDef (true, [ "s" ], newFparType (ConstChar, [ -1 ])) ]
  and writeString_f_rtype = Nothing in

  (* readInteger () : int *)
  let readInteger_fp_def_list = []
  and readInteger_f_rtype = RetDataType ConstInt in

  (* readChar () : char *)
  let readChar_fp_def_list = []
  and readChar_f_rtype = RetDataType ConstChar in

  (* readString (n : int; ref s : char[]) : nothing *)
  let readString_fp_def_list =
    [
      newFparDef (false, [ "n" ], newFparType (ConstInt, []));
      newFparDef (true, [ "s" ], newFparType (ConstChar, [ -1 ]));
    ]
  and readString_f_rtype = Nothing in

  (* ascii (c : char) : int *)
  let ascii_fp_def_list =
    [ newFparDef (false, [ "c" ], newFparType (ConstChar, [])) ]
  and ascii_f_rtype = RetDataType ConstInt in

  (* chr (n : int) : char *)
  let chr_fp_def_list =
    [ newFparDef (false, [ "n" ], newFparType (ConstInt, [])) ]
  and chr_f_rtype = RetDataType ConstChar in

  (* strlen (ref s : char[]) : int *)
  let strlen_fp_def_list =
    [ newFparDef (true, [ "s" ], newFparType (ConstChar, [ -1 ])) ]
  and strlen_f_rtype = RetDataType ConstInt in

  (* strcmp (ref s1, s2 : char[]) : int *)
  let strcmp_fp_def_list =
    [ newFparDef (true, [ "s1"; "s2" ], newFparType (ConstChar, [ -1 ])) ]
  and strcmp_f_rtype = RetDataType ConstInt in

  (* strcpy (ref trg, src : char[]) : nothing *)
  let strcpy_fp_def_list =
    [ newFparDef (true, [ "trg"; "src" ], newFparType (ConstChar, [ -1 ])) ]
  and strcpy_f_rtype = Nothing in

  (* strcat (ref trg, src : char[]) : nothing *)
  let strcat_fp_def_list =
    [ newFparDef (true, [ "trg"; "src" ], newFparType (ConstChar, [ -1 ])) ]
  and strcat_f_rtype = Nothing in

  let lib_list =
    [
      ("writeInteger", writeInteger_fp_def_list, writeInteger_f_rtype);
      ("writeChar", writeChar_fp_def_list, writeChar_f_rtype);
      ("writeString", writeString_fp_def_list, writeString_f_rtype);
      ("readInteger", readInteger_fp_def_list, readInteger_f_rtype);
      ("readChar", readChar_fp_def_list, readChar_f_rtype);
      ("readString", readString_fp_def_list, readString_f_rtype);
      ("ascii", ascii_fp_def_list, ascii_f_rtype);
      ("chr", chr_fp_def_list, chr_f_rtype);
      ("strlen", strlen_fp_def_list, strlen_f_rtype);
      ("strcmp", strcmp_fp_def_list, strcmp_f_rtype);
      ("strcpy", strcpy_fp_def_list, strcpy_f_rtype);
      ("strcat", strcat_fp_def_list, strcat_f_rtype);
    ]
  in
  List.iter define_lib_func lib_list

and set_stack_frames funcDef =
  let rec set_func_parents fd =
    let traverse_dfs child =
      match child with
      | L_funcDef c ->
          c.parent_func <- Some fd;
          set_func_parents c
      | _ -> ()
    in
    List.iter traverse_dfs fd.local_def_list
  in
  let set_stack_frame funcDef =
    (* take a func definition and do the following
       - create the stack frame containing the access link, parameters and local
         variables *)
    (* create the stack frame in field stack_frame for funcDef*)
    let stack_frame_ll =
      let frame_name =
        match funcDef.parent_func with
        | Some p -> funcDef.header.id
        | None -> "main"
      in
      named_struct_type context ("frame_" ^ frame_name)
    in
    funcDef.stack_frame <- Some stack_frame_ll;
    let access_link =
      match funcDef.parent_func with
      | Some p -> (
          let parent_stack_frame = p.stack_frame in
          match parent_stack_frame with
          | Some x ->
              let pointer = pointer_type x in
              funcDef.access_link <- Some pointer;
              [ pointer_type x ]
          | None -> [])
      | None -> []
    in

    let params_records = ref [] in
    let params_list = expand_fpar_def_list funcDef.header.fpar_def_list in
    let param_types_list = List.map lltype_of_fparDef params_list in
    (* gather local var definitions in a list for funcDef *)
    let params_length =
      match funcDef.parent_func with
      | Some _ -> List.length params_list + 1
      | None -> List.length params_list
    in

    let index = ref params_length in
    let vars_array = Array.of_list funcDef.local_def_list in

    Array.iteri
      (fun i var ->
        match var with
        | L_varDef v ->
            List.iter
              (fun id ->
                params_records := (id, !index, false) :: !params_records;
                incr index)
              v.id_list
        | _ -> ())
      vars_array;
    params_records := List.rev !params_records;
    funcDef.var_records <- funcDef.var_records @ !params_records;

    let var_types_list =
      let rec helper ld_list acc =
        match ld_list with
        | [] -> acc
        | hd :: tail -> (
            match hd with
            | L_varDef vd ->
                let vd_type =
                  lltype_of_t_type (t_type_of_varType vd.var_type)
                in
                let list_types = List.map (fun _ -> vd_type) vd.id_list in
                helper tail (acc @ list_types)
            | _ -> helper tail acc)
      in
      helper funcDef.local_def_list []
    in

    let stack_frame_records = access_link @ param_types_list @ var_types_list in
    let stack_frame_records_arr = Array.of_list stack_frame_records in
    struct_set_body stack_frame_ll stack_frame_records_arr false
  in
  set_func_parents funcDef;
  set_stack_frame funcDef;
  let rec iterate local_def =
    match local_def with
    | L_funcDef fd ->
        if Types.debugMode then Printf.printf "func in set %s\n%!" fd.header.id;
        set_stack_frame fd;
        List.iter iterate fd.local_def_list
    | _ -> ()
  in
  List.iter iterate funcDef.local_def_list

let add_opts pm =
  let opts =
    [
      add_ipsccp;
      add_memory_to_register_promotion;
      add_dead_arg_elimination;
      add_instruction_combination;
      add_cfg_simplification;
      add_function_inlining;
      add_function_attrs;
      add_scalar_repl_aggregation;
      add_early_cse;
      add_cfg_simplification;
      add_instruction_combination;
      add_tail_call_elimination;
      add_reassociation;
      add_loop_rotation;
      add_loop_unswitch;
      add_instruction_combination;
      add_cfg_simplification;
      add_ind_var_simplification;
      add_loop_idiom;
      add_loop_deletion;
      add_loop_unroll;
      add_gvn;
      add_memcpy_opt;
      add_sccp;
      add_licm;
      add_global_optimizer;
      add_global_dce;
      add_aggressive_dce;
      add_cfg_simplification;
      add_instruction_combination;
      add_dead_store_elimination;
      add_loop_vectorize;
      add_slp_vectorize;
      add_strip_dead_prototypes;
      add_global_dce;
      (* add_constant_propagation; *)
      add_cfg_simplification;
    ]
  in
  List.iter (fun f -> f pm) opts

and gen_on asts =
  (* Llvm_all_backends.initialize ();
     let triple = Target.default_triple () in
     set_target_triple triple the_module;
     let target = Target.by_triple triple in
     let machine = TargetMachine.create ~triple target in
     let dly = TargetMachine.data_layout machine in
     set_data_layout (DataLayout.as_string dly) the_module; *)
  define_lib_funcs ();
  set_stack_frames asts;
  gen_funcDef asts
(* let mpm = PassManager.create () in
   add_opts mpm;
   ignore (PassManager.run_module the_module mpm);
   assert_valid_module the_module *)
