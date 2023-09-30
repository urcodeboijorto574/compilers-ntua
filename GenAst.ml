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

let rec convert_to_llvm_type x = match x with
  | T_int -> i64_type context
  | ConstInt -> i64_type context
  | T_char -> i8_type context
  | ConstChar -> i8_type context
  | T_bool -> i1_type context
  | T_array (array_type, n) -> array_type convert_to_llvm (array_type) n
  | T_func -> (* to do here *)  
  | Nothing -> void_type context

let rec gen_expr expr = match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_char char_type x
  | E_lvalue lv -> (* to do here *)
  | E_func_call fc -> 
    let callee = fc.id and args = fc.expr_list in
    let callee = 
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in 
    let params = params callee in
      if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
      let args = Array.map gen_expr args in
      build_call callee args "calltmp" builder
  | E_sgn_exrp (sign, expr) -> 
      match sign with
      | O_plus -> gen_expr expr
      | O_minus -> build_neg (gen_expr expr) "minus" builder
  | E_op_expr_expr lhs oper rhs ->
      let lhs_val = gen_expr lhs in
      let rhs_lval = gen_expr rhs in
      match oper with
      | O_plus -> build_fadd lhs_val rhs_val "addtmp" builder
      | O_minus -> build_fsub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_lval "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_lval "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_lval "modtmp" builder
  | E_expr_parenthesized expr -> gen_expr expr

let rec gen_func_def func_header =
  let func_header.id = name and func_header.fpar_def_list = args in
  let args_list = List.map convert_to_llvm_type args in
  let args_array = Array.of_list args_list in
  let ret_type = convert_to_llvm_type func_header.ret_type in
  let ft = function_type ret_type args_array 
  let f = match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some -> failwith "redefinition of function (handled in semantic analysis)"
in
  Array.iteri (fun i a ->
  let n = args.(i) in
  set_value_name n a;
  Hashtbl.add named_values n a;
) (params f);
f

let gen_func_code = function
    | 

  (* let iterate list = match list with
  | [] -> Array.make 0 lltype
  | hd :: tail -> match hd.fpar_type.data_type with
    | ConstInt -> Array.make (List.length hd) int_type
    | ConstChar ->  Array.make (List.length hd) char_type  *) 

