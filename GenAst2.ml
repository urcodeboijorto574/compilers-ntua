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
let named_functions = Hashtbl.create 2000

let rec convert_to_llvm_type x = match x with
  | T_int -> int_type
  | ConstInt -> int_type
  | T_char -> char_type
  | ConstChar -> char_type
  | T_bool -> bool_type
  | T_array (t, n) -> array_type (convert_to_llvm t) n
  | T_func -> failwith "TODO" (* It's pointless to define something for functions here  *)
  | Nothing -> void_type context

let rec convert_param_to_llvm_type x = match x.ref with
  | false -> convert_to_llvm_type x.fpar_type.data_type
  | true -> (* TODO: need to add a check here whether type is
     array. Maybe need to change fparType and add type array there *)
     pointer_type (convert_to_llvm_type x)


(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)

 (* TODO: t_type may be an array type*)
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

  (* Create array of parameters. This array is of like:
     [llvm int, llvm char, llvm int *, ...]*)
let gen_func_prototype header = 
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  (* edw to in sunexizetai??*)
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  let ret_type = header.ret_type in
  let param_types_list = List.iter convert_param_to_llvm_type args in
  let param_types_array = Array.of_list param_types_list in
  let return_type = convert_to_llvm_type ret_type in
  let ft = function_type return_type param_types_array in
  let f = match lookup_function name thee_module with
    | None -> declare_function name ft thee_module
    | Some -> failwith "semantic analysis error: function already defined"
  in
  (* Set names for all arguments. *)
  Array.iteri (fun i a ->
    let n = param_types_array.(i).(List.hd id_list) in
    (* Set the name of each argument which is an llvalue, to a string *)
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
  ignore(gen_func_prototype the_func.header);
  ignore(create_argument_allocas the_func the_func.header);

  let bb = append_block context "entry" the_func in
  position_at_end builder;
  List.iter 
  (fun local_def -> 
    match local_def with
    | L_varDef v ->
       List.iter (fun x -> create_entry_block_alloca the_func x v.var_type.data_type) 
       v.id_list
    
    | L_funcDef fd -> gen_func fd
    | L_funcDecl fdl -> () (* TODO: Function Declarations *)
  ) the_func.local_def_list;

  let stmt_list = match the_func.block with
  | Block b -> b
  | _ -> () in
  List.iter gen_stmt stmt_list;



and gen_expr expr ?(is_param_ref) = match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_char char_type x

  | E_lvalue lv -> match lv with
    | L_id id -> let lv_addr = Hashtbl.find named_values id in
      match is_param_ref with
        | false -> build_load lv_addr id builder
        | true -> lv_addr
    | L_string -> failwith "argument cannot be of type string"
    | L_comp (lv2, expr2) -> () (* TODO *)

  | E_func_call fc -> 
    (* get this list here:
      [ {ref, [a], int}, {ref, [b], int}, {ref, [c], int}, {noref, [e], char}, {noref, [f], char} ]  *)
    let fpar_def_list = Hashtbl.find named_functions (Hashtbl.hash fc.id) in
    (* TODO: may need some work here *)
    let callee = fc.id and args_list = fc.expr_list in
    let callee = 
      match lookup_function callee thee_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in 
    let params = params callee in
      if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
    let i = ref 0 in
    let args = List.map 
    (fun x -> 
      let ith_elem = List.nth args_list !i in
      if x.ref = true then gen_expr ith_elem true 
      else gen_expr ith_elem false; incr i;) fpar_def_list in
    let args_array = Array.of_list args in
    build_call callee args_array "calltmp" builder

  | E_sgn_expr (sign, expr) -> (
      match sign with
      | O_plus -> gen_expr expr
      | O_minus -> build_neg (gen_expr expr) "minus" builder)
  | E_op_expr_expr (lhs, oper, rhs) ->
      let lhs_val = gen_expr lhs in
      let rhs_val = gen_expr rhs in
      match oper with
      | O_plus -> build_add lhs_val rhs_val "addtmp" builder
      | O_minus -> build_sub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_val "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_val "modtmp" builder
  | E_expr_parenthesized expr -> gen_expr expr


and gen_stmt stmt = 
  match stmt with
  | S_assignment (lv, expr) -> 
    match lv with  
    | L_id id -> 
      let lv_addr = Hashtbl.find named_values id in
      let value = gen_expr expr in
      ignore(build_store value lv_addr)
  | _ -> ()




