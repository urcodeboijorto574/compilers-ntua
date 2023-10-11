open Llvm
open Ast
open Symbol
open Types

exception Error of string

let context = global_context ()
let thee_module = create_module context "my_module"
let builder = builder context
let int_type = i64_type context
let char_type = i8_type context
let bool_type = i1_type context

(* Symbol table that holds the memory location of the variable in question*)
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 2000

let rec convert_to_llvm_type x = match x with
  | T_int -> i64_type context
  | ConstInt -> i64_type context
  | T_char -> i8_type context
  | ConstChar -> i8_type context
  | T_bool -> i1_type context
  | T_array (t, n) -> array_type (convert_to_llvm t) n
  | T_func -> failwith "TODO" (* It's pointless to define something for functions here  *)
  | Nothing -> void_type context

let rec convert_param_to_llvm_type x = match x.ref with
  | true -> convert_to_llvm_type x.fpar_type.data_type
  | false -> match x. (* TODO: need to add a check here whether type is
     array. Maybe need to change fparType and add type array there *)
     pointer_type (convert_to_llvm_type x)


(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name t_type =
  let builder = builder_at (instr_begin (entry_block the_function)) in
  build_alloca t_type var_name builder

(* Currently fparDef does not help a lot
   [ {ref, [a, b, c], int}, {noref, [e,f], char} ] -->
   [ {ref, a, int}, {ref, b, int}, {ref, c, int}, {noref, e, char}, {noref, f, char} ] *)
let expand_fpar_def_list (def_list : fparDef list) : fparDef list =
  let expand_fpar_def def =
    List.map (fun id -> {ref = def.ref; id_list = [id]; fpar_type = def.fpar_type}) def.id_list
  in
  List.concat (List.map expand_fpar_def def_list)

let gen_func_prototype header = 
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  let ret_type = header.ret_type in
  let param_types_list = List.iter convert_param_to_llvm_type args in
  let param_types_array = Array.of_list param_types_list in
  let return_type = convert_param_to_llvm_type ret_type in
  let ft = function_type return_type param_types_array in
  let f = match lookup_function name thee_module with
    | None -> declare_function name ft thee_module
    | Some -> failwith "semantic analysis error: function already defined"
  in
  (* Set names for all arguments. *)
  Array.iteri (fun i a ->
    let n = param_types_array.(i).(List.hd id_list) in
    (* Set the name of each argument which is an llvalue to a string *)
    set_value_name n a;
    Hashtbl.add named_values n a;
  ) (params f);
  f

(* Create an alloca for each argument and register the argument in the symbol
* table so that references to it will succeed. *)
let create_argument_allocas the_function header =
  let args = expand_fpar_def_list header.fpar_def_list in
  let param_types_list = List.iter convert_param_to_llvm_type args in
  let param_types_array = Array.of_list param_types_list in
  Array.iteri (fun i ai ->
    let ith_param = param_types_array.(i) in
    let var_name = ith_param.(List.hd id_list) in
    match ith_param.ref with
    (* if parameter NOT passed by ref, create alloca and add to symbol table *)
    | false -> 
      let alloca = create_entry_block_alloca the_function var_name in
      ignore(build_store ai alloca builder);
      Hashtbl.add named_values var_name alloca;
      (* if parameter passed by ref, don't create alloca, just add to the symbol table with
        address of var_name the value of the param. The value of the param is an address because of call by reference *)
    | true -> Hashtbl.add named_values var_name ai;
  ) (params the_function)

let gen_func the_func = 
  let rec helper f local_def = match local_def with
    | L_varDef v



let rec gen_expr expr = match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_char char_type x
  | E_lvalue lv -> failwith "TODO" (* TODO *)
  | E_func_call fc -> 
    let callee = fc.id and args = fc.expr_list in
    let callee = 
      match lookup_function callee thee_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in 
    let params = params callee in
      if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
      let args = Array.map gen_expr args in
      build_call callee args "calltmp" builder
  | E_sgn_exrp (sign, expr) -> (
      match sign with
      | O_plus -> gen_expr expr
      | O_minus -> build_neg (gen_expr expr) "minus" builder)
  | E_op_expr_expr (lhs, oper, rhs) ->
      let lhs_val = gen_expr lhs in
      let rhs_lval = gen_expr rhs in
      match oper with
      | O_plus -> build_fadd lhs_val rhs_val "addtmp" builder
      | O_minus -> build_fsub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_lval "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_lval "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_lval "modtmp" builder
  | E_expr_parenthesized expr -> gen_expr expr


