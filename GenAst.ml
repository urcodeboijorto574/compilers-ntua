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

(* Symbol table that holds the memory location of the variable in question*)
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 2000
let named_functions = Hashtbl.create 2000

let rec convert_to_llvm_type x =
  match x with
  | T_int -> int_type
  | ConstInt -> int_type
  | T_char -> char_type
  | ConstChar -> char_type
  | T_bool -> bool_type
  | T_array (t, n) -> array_type (convert_to_llvm_type t) n
  | T_func t -> convert_to_llvm_type t
  | T_none -> void_type context

let rec convert_param_to_llvm_type x =
  let t_type = Types.t_type_of_dataType x.fpar_type.data_type in
  match x.ref with
  | false -> convert_to_llvm_type t_type
  | true ->
      (* TODO: need to add a check here whether type is
         array. Maybe need to change fparType and add type array there *)
      pointer_type (convert_to_llvm_type t_type)

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)

(* TODO: t_type may be an array type*)
let create_entry_block_alloca the_function var_name t_type =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca t_type var_name builder

(* Currently fparDef does not help a lot
   [ {ref, [a, b, c], int}, {noref, [e,f], char} ] -->
   [ {ref, [a], int}, {ref, [b], int}, {ref, [c], int}, {noref, [e], char}, {noref, [f], char} ] *)
let expand_fpar_def_list (def_list : fparDef list) : fparDef list =
  let expand_fpar_def def =
    List.map
      (fun id -> { ref = def.ref; id_list = [ id ]; fpar_type = def.fpar_type })
      def.id_list
  in
  List.concat (List.map expand_fpar_def def_list)

(* Create array of parameters. This array is of like:
   [llvm int, llvm char, llvm int *, ...]*)
let gen_func_prototype (header : Ast.header) =
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  let args_array = Array.of_list args in
  (* edw to in sunexizetai??*)
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  let ret_type = header.ret_type in
  let param_types_list = List.map convert_param_to_llvm_type args in
  let param_types_array = Array.of_list param_types_list in
  let return_type = convert_to_llvm_type (Types.t_type_of_retType ret_type) in
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
  f

(* Create an alloca for each argument and register the argument in the symbol
   * table so that references to it will succeed. *)
let rec create_argument_allocas the_function header =
  let args = expand_fpar_def_list header.fpar_def_list in
  let args_array = Array.of_list args in
  let param_types_list = List.map convert_param_to_llvm_type args in
  let param_types_array = Array.of_list param_types_list in
  Array.iteri
    (fun i ai ->
      let ith_param = args_array.(i) in
      let var_name =
        match ith_param.id_list with
        | [ id ] -> id
        | _ -> failwith "error in list"
      in
      match ith_param.ref with
      (* if parameter NOT passed by ref, create alloca and add to symbol table *)
      | false ->
          let ith_param_ll_type =
            convert_to_llvm_type
              (Types.t_type_of_dataType ith_param.fpar_type.data_type)
          in
          let alloca =
            create_entry_block_alloca the_function var_name ith_param_ll_type
          in
          ignore (build_store ai alloca builder);
          Hashtbl.add named_values var_name alloca
          (* if parameter passed by ref, don't create alloca, just add to the symbol table with
             address of var_name the value of the param. The value of the param is an address because of call by reference *)
      | true -> Hashtbl.add named_values var_name ai)
    (params the_function)

and gen_func the_func =
  (* iterate functions in dfs order *)
  let iterate x = match x with L_funcDef f -> gen_func f | _ -> () in
  List.iter iterate the_func.local_def_list;
  (* let the_func_ll =
       match lookup_function the_func.header.id the_module with
       | Some f -> f
       | None -> failwith "undeclared function"
     in *)
  let the_func_ll = gen_func_prototype the_func.header in
  let bb = append_block context "entry" the_func_ll in
  position_at_end bb builder;
  ignore (create_argument_allocas the_func_ll the_func.header);

  List.iter
    (fun local_def ->
      match local_def with
      | L_varDef v ->
          let ll_type =
            convert_to_llvm_type (Types.t_type_of_dataType v.var_type.data_type)
          in
          List.iter
            (fun x ->
              let alloca = create_entry_block_alloca the_func_ll x ll_type in
              Hashtbl.add named_values x alloca)
            v.id_list
      | L_funcDef fd -> ()
      | L_funcDecl fdl -> failwith "todo" (* TODO: Function Declarations *))
    the_func.local_def_list;

  let stmt_list =
    match the_func.block with Block b -> b | _ -> failwith "todofbf"
  in
  List.iter gen_stmt stmt_list;
  if block_terminator @@ insertion_block builder = None then
    ignore (build_ret_void builder)

and gen_expr expr ?(is_param_ref : bool option) =
  match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_int char_type (int_of_char x)
  | E_lvalue lv -> (
      match lv with
      | L_id id -> (
          let lv_addr = Hashtbl.find named_values id in
          match is_param_ref with
          | Some ref ->
              if ref = false then build_load lv_addr id builder else lv_addr
          | None -> build_load lv_addr id builder)
      | L_string s ->
          (* pointer_type (const_string context s) *)
          (* create const string *)
          let const_str = const_stringz context s in
          (* create variable holding  const_str*)
          let string_var = define_global "string_var" const_str the_module in
          (* point to the first element (char) of the string *)
          let first_element = const_int int_type 0 in
          build_gep string_var
            [| first_element; first_element |]
            "string_ptr" builder
      | L_comp (lv2, expr2) -> failwith "todo" (* TODO *))
  | E_func_call fc ->
      (* get this list here:
         [ {ref, [a], int}, {ref, [b], int}, {ref, [c], int}, {noref, [e], char}, {noref, [f], char} ] *)
      let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
      (* TODO: may need some work here *)
      let callee = fc.id in
      let args_list = fc.expr_list in
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let params = params callee in
      (* if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed"); *)
      let i = ref 0 in
      let res = ref [] in
      let args =
        List.iter
          (fun x ->
            let ith_elem = List.nth args_list !i in
            if x.ref = true then
              res := gen_expr ith_elem ~is_param_ref:true :: !res
            else
              res := gen_expr ith_elem ~is_param_ref:false :: !res;
            incr i)
          fpar_def_list
      in
      (*List.iter(fun x -> Printf.printf("%d\n", x) ) !res;*)
      let args_array = Array.of_list (List.rev !res) in
      build_call callee args_array "calltmp" builder
  | E_sgn_expr (sign, expr) -> (
      match sign with
      | O_plus -> gen_expr expr ~is_param_ref:false
      | O_minus -> build_neg (gen_expr expr ~is_param_ref:false) "minus" builder
      )
  | E_op_expr_expr (lhs, oper, rhs) -> (
      let lhs_val = gen_expr lhs ~is_param_ref:false in
      let rhs_val = gen_expr rhs ~is_param_ref:false in
      match oper with
      | O_plus -> build_add lhs_val rhs_val "addtmp" builder
      | O_minus -> build_sub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_val "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_val "modtmp" builder)
  | E_expr_parenthesized expr -> gen_expr expr ~is_param_ref:false

and gen_stmt stmt =
  match stmt with
  | S_assignment (lv, expr) -> (
      match lv with
      | L_id id ->
          let lv_addr = Hashtbl.find named_values id in
          let value = gen_expr expr ~is_param_ref:false in
          ignore (build_store value lv_addr builder)
      | _ -> failwith "tododd")
  | S_func_call fc ->
      (* let params_array = [| int_type; |] in
         build_call "writeInteger" params_array "calltmp" builder *)
      (* get this list here:
         [ {ref, [a], int}, {ref, [b], int}, {ref, [c], int}, {noref, [e], char}, {noref, [f], char} ] *)
      let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
      (* TODO: may need some work here *)
      let callee = fc.id in
      let args_list = fc.expr_list in
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let params = params callee in
      (* if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed"); *)
      let i = ref 0 in
      let res = ref [] in
      let args =
        List.iter
          (fun x ->
            let ith_elem = List.nth args_list !i in
            if x.ref = true then
              res := gen_expr ith_elem ~is_param_ref:true :: !res
            else
              res := gen_expr ith_elem ~is_param_ref:false :: !res;
            incr i)
          fpar_def_list
      in
      let args_array = Array.of_list !res in
      ignore (build_call callee args_array "" builder)
  | S_return expr -> (
      match expr with
      | None -> ignore (build_ret_void builder)
      | Some e ->
          let ll_expr = gen_expr e ~is_param_ref:false in
          ignore (build_ret ll_expr builder))
  | _ -> failwith "todoaa"

(* and define_lib_func =
   let fp_type = newFparType (ConstInt, []) in
   let fp_def = newFparDef (false, [ "n" ], fp_type) in
   let writeInteger_header =
     newHeader ("writeInteger", [ fp_def ], Ast.Nothing)
   in
   gen_func_prototype writeInteger_header *)

(* and define_lib_func (fp_type, fp_def, f_id, f_rtype) =
   let fun_header = newHeader(f_id, [ fp_def ], f_rtype)
   in
   ignore(gen_func_prototype fun_header) *)

let define_lib_funcs =
  let define_lib_func (fp_def_list, f_id, f_rtype) =
    let fun_header = newHeader (f_id, fp_def_list, f_rtype) in
    ignore (gen_func_prototype fun_header)
  in

  let writeInteger_fp_type = newFparType (ConstInt, []) in
  let writeInteger_fp_def_list =
    [ newFparDef (false, [ "n" ], writeInteger_fp_type) ]
  in
  let writeInteger_f_rtype = Ast.Nothing in

  let writeChar_fp_type = newFparType (ConstChar, []) in
  let writeChar_fp_def_list =
    [ newFparDef (false, [ "c" ], writeChar_fp_type) ]
  in
  let writeChar_f_rtype = Ast.Nothing in

  let writeString_fp_type = newFparType (ConstChar, [ -1 ]) in
  let writeString_fp_def_list =
    [ newFparDef (true, [ "s" ], writeString_fp_type) ]
  in
  let writeString_f_rtype = Ast.Nothing in

  (* this value is dummy *)
  (* let readInteger_fp_type = newFparType (Nothing, []) in *)
  let readInteger_fp_def_list = [] in
  let readInteger_f_rtype = Ast.RetDataType Ast.ConstInt in

  let lib_list =
    [
      (writeInteger_fp_def_list, "writeInteger", writeInteger_f_rtype);
      (writeChar_fp_def_list, "writeChar", writeChar_f_rtype);
      (writeString_fp_def_list, "writeString", writeString_f_rtype);
      (readInteger_fp_def_list, "readInteger", readInteger_f_rtype);
    ]
  in
  List.iter define_lib_func lib_list
(* let writeString_fp_type = newFparType (ConstChar, [-1]) in
   let writeString_fp_def = newFparDef (true, [ "s" ], writeString_fp_type) in
   let writeString_f_rtype = Ast.Nothing in
   let _ = define_lib_func writeString_fp_type writeString_fp_def "writeString" writeString_f_rtype in
   () *)

let gen_on asts =
  ignore define_lib_funcs;
  gen_func asts
