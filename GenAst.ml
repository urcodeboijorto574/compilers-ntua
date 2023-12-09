open Llvm
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
let lib_function_names = [ "writeInteger"; "writeString"; "writeByte" ]

let build_nop () =
  let zero = const_int bool_type 0 in
  build_add zero zero "nop" builder

(* Symbol table that holds the memory location of the variable in question*)
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 2000
let named_functions = Hashtbl.create 2000
let blocks_list = ref []

let rec llvm_type_of_t_type x =
  match x with
  | T_int -> int_type
  | T_char -> char_type
  | T_array (t, n) -> array_type (llvm_type_of_t_type t) n
  | T_func t -> llvm_type_of_t_type t
  | T_none -> void_type context

and llvm_type_of_param x =
  let t_type = Types.t_type_of_dataType x.fpar_type.data_type in
  match x.ref with
  | false -> llvm_type_of_t_type t_type
  | true ->
      (* TODO: need to add a check here whether type is
         array. Maybe need to change fparType and add type array there *)
      pointer_type (llvm_type_of_t_type t_type)

(* Currently fparDef does not help a lot
   [ {ref, [a, b, c], int}, {noref, [e,f], char} ] -->
   [ {ref, [a], int}, {ref, [b], int}, {ref, [c], int}, {noref, [e], char}, {noref, [f], char} ] *)
and expand_fpar_def_list (def_list : fparDef list) : fparDef list =
  let expand_fpar_def def =
    List.map
      (fun id -> { ref = def.ref; id_list = [ id ]; fpar_type = def.fpar_type })
      def.id_list
  in
  List.concat (List.map expand_fpar_def def_list)

and gen_lvalue_address id stack_frame_alloca funcDef stack_frame_length =
  let rec iterate i stack_frame funcDef =
    let tuple = List.nth funcDef.var_records i in
    let var_name = match tuple with v, _, _ -> v in
    let elem_pos = match tuple with _, p, _ -> p in
    let is_ref = match tuple with _, _, ref -> ref in
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

and gen_expr is_param_ref stack_frame_alloca stack_frame_length funcDef expr =
  match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_int char_type (int_of_char x)
  | E_lvalue lv -> (
      match lv with
      | L_id id ->
          let lv_address =
            gen_lvalue_address id stack_frame_alloca funcDef stack_frame_length
          in
          if is_param_ref = false then
            build_load lv_address id builder
          else
            lv_address
      | L_comp (lv2, expr2) -> failwith "todo" (* TODO *)
      | _ -> failwith "tododd")
  | E_func_call fc ->
      let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
      (* TODO: may need some work here *)
      let callee = fc.id in
      let args_list = fc.expr_list in
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      ignore (params callee);
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
        if List.mem fc.id lib_function_names = false then
          Array.of_list ([ stack_frame_alloca ] @ rev_list)
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

and gen_cond stack_frame_alloca stack_frame_length funcDef = function
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

and gen_stmt stack_frame_alloca stack_frame_length funcDef stmt =
  match stmt with
  | S_assignment (lv, expr) -> (
      match lv with
      | L_id id ->
          let lv_address =
            gen_lvalue_address id stack_frame_alloca funcDef stack_frame_length
          in
          let lv_value =
            gen_expr false stack_frame_alloca stack_frame_length funcDef expr
          in
          build_store lv_value lv_address builder
      | _ -> failwith "tododd")
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
        if List.mem fc.id lib_function_names = false then
          Array.of_list ([ stack_frame_alloca ] @ rev_list)
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
      let result_llvalue = build_br merge_basic_block builder in
      position_at_end merge_basic_block builder;
      result_llvalue
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
      build_nop ()
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
      let result_llvalue =
        build_cond_br (cond_val ()) while_basic_block cont_basic_block builder
      in

      position_at_end cont_basic_block builder;
      result_llvalue
  | S_return expr_opt -> (
      match expr_opt with
      | None -> build_ret_void builder
      | Some e ->
          let ll_expr =
            gen_expr false stack_frame_alloca stack_frame_length funcDef e
          in
          build_ret ll_expr builder)
  | S_semicolon -> build_nop ()

and gen_header (header : Ast.header) access_link =
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  ignore (Array.of_list args);
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  let ret_type = header.ret_type in
  let access_link_list =
    match access_link with Some x -> [ x ] | None -> []
  in

  let param_types_list = access_link_list @ List.map llvm_type_of_param args in
  let param_types_array = Array.of_list param_types_list in
  let return_type = llvm_type_of_t_type (Types.t_type_of_retType ret_type) in
  let ft = function_type return_type param_types_array in
  let f =
    match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some x -> failwith "semantic analysis error: function already defined"
  in
  f

and gen_funcDef funcDef =
  let funcDef_ll = gen_header funcDef.header funcDef.access_link in
  let bb = append_block context "entry" funcDef_ll in
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
  let stack_frame_alloca =
    build_alloca stack_frame_type ("stack_frame_" ^ funcDef.header.id) builder
  in
  let args = expand_fpar_def_list funcDef.header.fpar_def_list in
  let args_array = Array.of_list args in
  let access_link_list =
    match funcDef.access_link with Some al -> [ al ] | None -> []
  in
  Array.iteri
    (fun i ai ->
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
              match ith_param.id_list with
              | [ id ] -> id
              | _ -> failwith "error in list"
            in
            set_value_name var_name position;
            params_records := (var_name, i, ith_param.ref) :: !params_records;
            ignore (build_store ai position builder)
        | list ->
            set_value_name "access_link" position;
            params_records := ("access_link", i, false) :: !params_records;
            ignore (build_store ai position builder))
      else
        let ith_param =
          match access_link_list with
          | [] -> args_array.(i)
          | list -> args_array.(i - 1)
        in
        let var_name =
          match ith_param.id_list with
          | [ id ] -> id
          | _ -> failwith "error in list"
        in
        set_value_name var_name position;
        params_records := (var_name, i, ith_param.ref) :: !params_records;
        ignore (build_store ai position builder))
    (params funcDef_ll);
  params_records := List.rev !params_records;
  funcDef.var_records <- !params_records @ funcDef.var_records;
  funcDef.stack_frame_addr <- Some stack_frame_alloca;

  let struct_index = ref (Array.length (params funcDef_ll)) in
  (* iterate functions in dfs order *)
  let iterate local_def =
    match local_def with
    | L_varDef v ->
        ignore
          (llvm_type_of_t_type (Types.t_type_of_dataType v.var_type.data_type));
        ignore (List.nth v.id_list 0);
        List.iter
          (fun x ->
            let position =
              build_struct_gep stack_frame_alloca !struct_index x builder
            in
            set_value_name x position;
            incr struct_index)
          v.id_list
    | L_funcDef fd -> gen_funcDef fd
    | L_funcDecl fdl -> failwith "todo" (* TODO: Function Declarations *)
  in
  List.iter iterate funcDef.local_def_list;

  ignore (List.length funcDef.var_records);
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

let define_lib_funcs =
  let gen_header_lib (header : Ast.header) =
    let name = header.id in
    let args = expand_fpar_def_list header.fpar_def_list in
    let args_array = Array.of_list args in
    Hashtbl.add named_functions (Hashtbl.hash name) args;
    let ret_type = header.ret_type in
    let param_types_list = List.map llvm_type_of_param args in
    let param_types_array = Array.of_list param_types_list in
    let return_type = llvm_type_of_t_type (Types.t_type_of_retType ret_type) in
    let ft = function_type return_type param_types_array in
    let f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some x -> failwith "semantic analysis error: function already defined"
    in
    (* Set names for all arguments. *)
    Array.iteri
      (fun i a ->
        let n =
          match args_array.(i).id_list with
          | [ id ] -> id
          (* will never reach here, becaues id_list has certainly only one element *)
          | _ -> failwith "error in list"
        in
        (* Set the name of each argument which is an llvalue, to a string *)
        set_value_name n a;
        Hashtbl.add named_values n a)
      (params f);
    f (* why is this 'f' alone here? *)
  in
  let define_lib_func (fp_def_list, f_id, f_rtype) =
    let fun_header = newHeader (f_id, fp_def_list, f_rtype) in
    ignore (gen_header_lib fun_header)
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
      (writeInteger_fp_def_list, "writeInteger", writeInteger_f_rtype);
      (writeChar_fp_def_list, "writeChar", writeChar_f_rtype);
      (writeString_fp_def_list, "writeString", writeString_f_rtype);
      (readInteger_fp_def_list, "readInteger", readInteger_f_rtype);
      (readChar_fp_def_list, "readChar", readChar_f_rtype);
      (readString_fp_def_list, "readString", readString_f_rtype);
      (ascii_fp_def_list, "ascii", ascii_f_rtype);
      (chr_fp_def_list, "chr", chr_f_rtype);
      (strlen_fp_def_list, "strlen", strlen_f_rtype);
      (strcmp_fp_def_list, "strcmp", strcmp_f_rtype);
      (strcpy_fp_def_list, "strcpy", strcpy_f_rtype);
      (strcat_fp_def_list, "strcat", strcat_f_rtype);
    ]
  in
  List.iter define_lib_func lib_list
(* let writeString_fp_type = newFparType (ConstChar, [-1]) in
   let writeString_fp_def = newFparDef (true, [ "s" ], writeString_fp_type) in
   let writeString_f_rtype = Ast.Nothing in
   ignore (
     define_lib_func writeString_fp_type writeString_fp_def "writeString"
       writeString_f_rtype) *)

(* set parent function of each function *)
let rec set_func_parents fd =
  let traverse_dfs child =
    match child with
    | L_funcDef c ->
        c.parent_func <- Some fd;
        set_func_parents c
    | _ -> ()
  in
  List.iter traverse_dfs fd.local_def_list

(* take a func definition and do the following
   - create the stack frame containing the access link, parameters and local variables *)
and set_stack_frame funcDef =
  (* create the stack frame in field stack_frame for funcDef*)
  let stack_frame_ll =
    named_struct_type context ("frame_" ^ funcDef.header.id)
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
  let param_types_list = List.map llvm_type_of_param params_list in
  (* gather local var definitions in a list for funcDef *)
  let params_length =
    match funcDef.header.id with
    | "main" -> List.length params_list
    | _ -> List.length params_list + 1
  in
  ignore (match funcDef.access_link with Some al -> [ al ] | None -> []);

  let index = ref params_length in
  let vars_array = Array.of_list funcDef.local_def_list in

  Array.iteri
    (fun i ai ->
      match ai with
      | L_varDef v ->
          List.iter
            (fun x ->
              params_records := (x, !index, false) :: !params_records;
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
                llvm_type_of_t_type (t_type_of_dataType vd.var_type.data_type)
              in
              let list_types = List.map (fun x -> vd_type) vd.id_list in
              helper tail (acc @ list_types)
          | _ -> helper tail acc)
    in
    helper funcDef.local_def_list []
  in

  let stack_frame_records = access_link @ param_types_list @ var_types_list in
  let stack_frame_records_arr = Array.of_list stack_frame_records in
  struct_set_body stack_frame_ll stack_frame_records_arr false

and set_stack_frames funcDef =
  set_func_parents funcDef;
  set_stack_frame funcDef;
  let rec iterate local_def =
    match local_def with
    | L_funcDef fd ->
        set_stack_frame fd;
        List.iter iterate fd.local_def_list
    | _ -> ()
  in
  List.iter iterate funcDef.local_def_list

and gen_on asts =
  define_lib_funcs;
  set_stack_frames asts;
  gen_funcDef asts
