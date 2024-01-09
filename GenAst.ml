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
let current_func_id : string list ref = ref []

let rec lltype_of_t_type x =
  match x with
  | T_int -> int_type
  | T_char -> char_type
  | T_array (t, n) ->
      pointer_type (lltype_of_t_type (Types.final_t_type_of_t_array t))
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
  | true -> pointer_type (lltype_of_t_type t_type)

and lltype_of_varDef vd =
  let result =
    lltype_of_t_type (Ast.t_type_of_dataType vd.var_type.data_type)
  in
  let isArray = vd.var_type.array_dimensions <> [] in
  if isArray then pointer_type result else result

let rec expand_fpar_def_list (def_list : fparDef list) : fparDef list =
  let expand_fpar_def def =
    List.map
      (fun id -> { ref = def.ref; id_list = [ id ]; fpar_type = def.fpar_type })
      def.id_list
  in
  List.concat (List.map expand_fpar_def def_list)

and expand_var_def_list (vdl : Ast.varDef list) : Ast.varDef list =
  let expand_var_def (vd : Ast.varDef) : Ast.varDef list =
    List.map (fun id -> { id_list = [ id ]; var_type = vd.var_type }) vd.id_list
  in
  List.concat (List.map expand_var_def vdl)

and gen_funcCall stackFrame (fc : Ast.funcCall) =
  let stack_frame_alloca = Option.get stackFrame.stack_frame_addr in
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
      res := gen_expr fpar_def.ref stackFrame ith_elem :: !res;
      incr i)
    fpar_def_list;
  let rev_list = List.rev !res in
  let args_array =
    let first_argument =
      if fc.id = List.hd !current_func_id then begin
        let access_link_ptr =
          build_struct_gep stack_frame_alloca 0 "access_link_ptr" builder
        in
        build_load access_link_ptr "access_link_ptr" builder
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

and gen_lvalue stackFrame lv =
  let stack_frame_alloca = Option.get stackFrame.stack_frame_addr in
  let gen_lvalue_address id =
    let rec iterate i stack_frame_alloca stackFrame =
      if i >= stackFrame.stack_frame_length then
        match stackFrame.parent_stack_frame with
        | Some sfParent ->
            let access_link =
              build_struct_gep
                (Option.get stackFrame.stack_frame_addr)
                0 "access_link_ptr" builder
            in
            let parentStackFrameAddr =
              build_load access_link "access_link_val" builder
            in
            iterate 0 parentStackFrameAddr sfParent
        | None -> failwith "variable not found"
      else
        let var_name, elem_pos, is_ref, is_array =
          List.nth stackFrame.var_records i
        in
        if var_name = id then begin
          let addr =
            build_struct_gep stack_frame_alloca elem_pos var_name builder
          in
          if is_ref = false then begin
            match is_array with
            | true -> build_load addr (var_name ^ "_address") builder
            | false -> addr
          end
          else
            build_load addr (var_name ^ "_address") builder
        end
        else
          iterate (i + 1) stack_frame_alloca stackFrame
    in
    let initialIndex = if stackFrame.parent_stack_frame = None then 0 else 1 in
    iterate initialIndex stack_frame_alloca stackFrame
  in
  match lv.lv_kind with
  | L_id id -> gen_lvalue_address id
  | L_string s ->
      let const_str = const_stringz context s in
      let string_var = define_global "string_var" const_str the_module in
      let zero = const_int int_type 0 in
      build_gep string_var [| zero; zero |] "string_ptr" builder
  | L_comp _ -> begin
      let arrayPtr =
        let rec gen_lvalue_kind = function
          | L_id id -> gen_lvalue_address id
          | L_string s ->
              gen_lvalue stackFrame { lv_kind = L_string s; lv_type = None }
          | L_comp (lvk, _) -> gen_lvalue_kind lvk
        in
        gen_lvalue_kind lv.lv_kind
      in
      let index =
        let dimensions : Llvm.llvalue list =
          let rec get_dimensions : Types.t_type -> int list = function
            | T_func _ | T_none -> assert false
            | T_int | T_char -> []
            | T_array (typ, size) -> size :: get_dimensions typ
          in
          let dimensionsList : int list =
            let arrayType = Option.get (Option.get lv.lv_type).array_type in
            List.rev (get_dimensions arrayType)
          in
          List.map (const_int int_type) dimensionsList
        in
        let indices : Llvm.llvalue list =
          let rec get_indices : Ast.lvalue_kind -> Ast.expr list = function
            | L_id _ | L_string _ -> []
            | L_comp (lvk, i) -> i :: get_indices lvk
          in
          let indicesList : Ast.expr list = List.rev (get_indices lv.lv_kind) in
          List.map (gen_expr false stackFrame) indicesList
        in
        let indexExpr : Llvm.llvalue =
          let rec get_final_index :
              Llvm.llvalue list * Llvm.llvalue list -> Llvm.llvalue = function
            | [], _ | _, [] -> const_int int_type 0
            | i :: _, _ :: [] -> i
            | i :: itail, d :: dtail ->
                let product = build_mul i d "prod_temp" builder in
                build_add product
                  (get_final_index (itail, dtail))
                  "index_temp" builder
          in
          get_final_index (indices, dimensions)
        in
        build_intcast indexExpr int_type "index" builder
      in
      build_gep arrayPtr [| index |] "array_element_ptr" builder
    end

and gen_expr is_param_ref stackFrame expr =
  match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_int char_type (int_of_char x)
  | E_lvalue lv -> begin
      let lv_address = gen_lvalue stackFrame lv in
      if is_param_ref then
        lv_address
      else
        let var_name =
          match lv.lv_kind with
          | L_id id -> id
          | L_string _ | L_comp _ -> "array_element_val"
        in
        build_load lv_address var_name builder
    end
  | E_func_call fc -> gen_funcCall stackFrame fc
  | E_sgn_expr (sign, expr) -> (
      match sign with
      | O_plus -> gen_expr false stackFrame expr
      | O_minus -> build_neg (gen_expr false stackFrame expr) "minus" builder)
  | E_op_expr_expr (lhs, oper, rhs) -> (
      let lhs_val = gen_expr false stackFrame lhs in
      let rhs_val = gen_expr false stackFrame rhs in
      match oper with
      | O_plus -> build_add lhs_val rhs_val "addtmp" builder
      | O_minus -> build_sub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_val "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_val "modtmp" builder)
  | E_expr_parenthesized expr -> gen_expr false stackFrame expr

and gen_cond stackFrame = function
  | C_not_cond (lo, c) ->
      let ll_cond = gen_cond stackFrame c in
      build_not ll_cond "not" builder
  | C_cond_cond (c1, lo, c2) ->
      let lhs_val = gen_cond stackFrame c1 in
      let is_good, opName, build_instr =
        match lo with
        | O_and -> (const_int bool_type 0, "and", build_and)
        | O_or -> (const_int bool_type 1, "or", build_or)
        | O_not -> assert false
      in
      let start_basic_block = insertion_block builder in
      let function_bb = block_parent start_basic_block in
      let good_basic_block = append_block context "good" function_bb in
      let bad_basic_block = append_block context "bad" function_bb in
      let merge_basic_block =
        append_block context "good_bad_cont" function_bb
      in
      let result_bool () =
        build_icmp Llvm.Icmp.Eq lhs_val is_good
          (opName ^ "_short_circuit")
          builder
      in
      let resultCondition =
        build_alloca bool_type ("result_" ^ opName) builder
      in

      position_at_end start_basic_block builder;
      ignore
        (build_cond_br (result_bool ()) good_basic_block bad_basic_block builder);

      position_at_end good_basic_block builder;
      let resultGood = build_instr lhs_val is_good opName builder in
      ignore (build_store resultGood resultCondition builder);
      let new_good_basic_block = insertion_block builder in
      position_at_end new_good_basic_block builder;
      ignore (build_br merge_basic_block builder);

      position_at_end bad_basic_block builder;
      let rhs_val = gen_cond stackFrame c2 in
      let resultBad = build_instr lhs_val rhs_val opName builder in
      ignore (build_store resultBad resultCondition builder);
      let new_bad_basic_block = insertion_block builder in
      position_at_end new_bad_basic_block builder;
      ignore (build_br merge_basic_block builder);

      position_at_end merge_basic_block builder;
      build_load resultCondition (opName ^ "_result") builder
  | C_expr_expr (e1, co, e2) -> (
      let lhs_val = gen_expr false stackFrame e1 in
      let rhs_val = gen_expr false stackFrame e2 in
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
  | C_cond_parenthesized c -> gen_cond stackFrame c

and gen_stmt stackFrame stmt =
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
                try (Option.get lv.lv_type).elem_type
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
  | S_assignment (lv, expr) ->
      let lv_address = gen_lvalue stackFrame lv in
      let lv_value = gen_expr false stackFrame expr in
      build_store lv_value lv_address builder
  | S_func_call fc -> gen_funcCall stackFrame fc
  | S_block b -> begin
      match b with
      | Block [] -> build_nop ()
      | Block l ->
          let get_last_elem = function
            | [] -> assert false
            | l -> List.(hd (rev l))
          in
          get_last_elem (List.map (gen_stmt stackFrame) l)
    end
  | S_if (c, s) ->
      let start_basic_block = insertion_block builder in
      let function_bb = block_parent start_basic_block in
      let then_basic_block = append_block context "then" function_bb in
      let merge_basic_block = append_block context "if_then_cont" function_bb in

      position_at_end start_basic_block builder;
      let cond_val () =
        let zero = const_int bool_type 0 in
        let ll_c = gen_cond stackFrame c in
        build_icmp Ne zero ll_c "if_then_cond" builder
      in
      ignore
        (build_cond_br (cond_val ()) then_basic_block merge_basic_block builder);

      position_at_end then_basic_block builder;
      ignore (gen_stmt stackFrame s);
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
        let ll_c = gen_cond stackFrame c in
        build_icmp Ne zero ll_c "if_then_else_cond" builder
      in
      position_at_end start_basic_block builder;
      ignore
        (build_cond_br (cond_val ()) then_basic_block else_basic_block builder);

      position_at_end then_basic_block builder;
      ignore (gen_stmt stackFrame s1);
      let new_then_basic_block = insertion_block builder in
      position_at_end new_then_basic_block builder;
      ignore (build_br merge_basic_block builder);

      position_at_end else_basic_block builder;
      ignore (gen_stmt stackFrame s2);
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
        let ll_c = gen_cond stackFrame c in
        build_icmp Ne zero ll_c "while_cond" builder
      in
      position_at_end start_basic_block builder;
      ignore
        (build_cond_br (cond_val ()) while_basic_block cont_basic_block builder);
      position_at_end while_basic_block builder;
      ignore (gen_stmt stackFrame s);

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
          let ll_expr = gen_expr false stackFrame e in
          build_ret ll_expr builder)
  | S_semicolon -> build_nop ()

and gen_header (header : Ast.header) (access_link : Llvm.lltype option) =
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  match lookup_function name the_module with
  | None ->
      let name = if access_link = None then "main" else header.id in
      let ft =
        let return_type =
          lltype_of_t_type (Ast.t_type_of_retType header.ret_type)
        in
        let param_types_array =
          let param_types_list =
            Option.to_list access_link @ List.map lltype_of_fparDef args
          in
          Array.of_list param_types_list
        in
        function_type return_type param_types_array
      in
      declare_function name ft the_module
  | Some x ->
      (* TODO: Check whether this case should or should not be erroneous *)
      failwith "semantic analysis error: function already defined"

and gen_funcDef funcDef =
  let stackFrame = Option.get funcDef.stack_frame in
  let funcDef_ll = gen_header funcDef.header stackFrame.access_link in
  let bb = append_block context ("entry_" ^ funcDef.header.id) funcDef_ll in
  position_at_end bb builder;
  blocks_list := bb :: !blocks_list;
  let stack_frame_type = stackFrame.stack_frame_type in
  let stack_frame_alloca =
    let name =
      if funcDef.parent_func = None then "main" else funcDef.header.id
    in
    build_alloca stack_frame_type ("stack_frame_" ^ name) builder
  in
  let args_array =
    Array.of_list (expand_fpar_def_list funcDef.header.fpar_def_list)
  in

  (* Generation of fparDefs *)
  Array.iteri
    (fun i param ->
      let position =
        build_struct_gep stack_frame_alloca i "stack_frame_elem" builder
      in
      let isRootFunction = stackFrame.access_link = None in
      let isAccessLink = (not isRootFunction) && i = 0 in
      if isAccessLink then (
        let accessLink = param in
        set_value_name "access_link" position;
        ignore (build_store accessLink position builder))
      else
        let ith_param = args_array.(if isRootFunction then i else i - 1) in
        let var_name = try List.hd ith_param.id_list with _ -> assert false in
        set_value_name var_name position;
        ignore (build_store param position builder))
    (params funcDef_ll);

  stackFrame.stack_frame_addr <- Some stack_frame_alloca;

  (* Generation of local definitions *)
  let struct_index = ref (Array.length (params funcDef_ll)) in
  let rec iterate local_def =
    match local_def with
    | L_varDef v ->
        List.iter
          (fun id ->
            let position =
              build_struct_gep stack_frame_alloca !struct_index id builder
            in
            let isArray = v.var_type.array_dimensions <> [] in
            match isArray with
            | false ->
                set_value_name id position;
                incr struct_index
            | true ->
                let dimList = v.var_type.array_dimensions in
                let array_alloca =
                  let arraySize : Llvm.llvalue =
                    let productOfList =
                      List.fold_left (fun acc d -> acc * d) 1 dimList
                    in
                    const_int int_type productOfList
                  in
                  let t =
                    lltype_of_t_type
                      (Ast.t_type_of_dataType v.var_type.data_type)
                  in
                  build_array_alloca t arraySize "array_alloca" builder
                in
                set_value_name id position;
                incr struct_index;
                let arrayPtr =
                  let zero = const_int int_type 0 in
                  build_gep array_alloca [| zero |] "array_ptr" builder
                in
                ignore (build_store arrayPtr position builder))
          v.id_list
    | L_funcDef fd -> gen_funcDef fd
    | L_funcDecl fdl -> failwith "TODO gen_funcDef: iterate (L_funcDecl _)"
  in
  List.iter iterate funcDef.local_def_list;

  (* Generation of block *)
  current_func_id := funcDef.header.id :: !current_func_id;
  let stmt_list = match funcDef.block with Block b -> b in
  ignore (List.map (gen_stmt stackFrame) stmt_list);
  current_func_id := List.tl !current_func_id;

  if block_terminator @@ insertion_block builder = None then
    ignore (build_ret_void builder);
  blocks_list :=
    List.tl !blocks_list (* if blocks_list = [] this will throw Failure _ *);
  if !blocks_list <> [] then
    position_at_end (List.hd !blocks_list) builder

and define_lib_funcs () =
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
  let set_stack_frame () =
    let parent_stack_frame =
      Option.map (fun fd -> Option.get fd.stack_frame) funcDef.parent_func
    in
    let access_link_opt : Llvm.lltype option =
      Option.map
        (fun parent_fd ->
          pointer_type (Option.get parent_stack_frame).stack_frame_type)
        funcDef.parent_func
    in
    let params_list : Ast.fparDef list =
      expand_fpar_def_list funcDef.header.fpar_def_list
    in
    let vars_list : Ast.varDef list =
      let rec extract_var_defs = function
        | [] -> []
        | L_varDef vd :: tail -> vd :: extract_var_defs tail
        | L_funcDef _ :: tail | L_funcDecl _ :: tail -> extract_var_defs tail
      in
      expand_var_def_list (extract_var_defs funcDef.local_def_list)
    in
    let stack_frame_type : Llvm.lltype =
      named_struct_type context ("frame_" ^ funcDef.header.id)
    in
    let stack_frame_records_arr : Llvm.lltype array =
      let stack_frame_records =
        let param_types_list = List.map lltype_of_fparDef params_list in
        let var_types_list = List.map lltype_of_varDef vars_list in
        Option.to_list access_link_opt @ param_types_list @ var_types_list
      in
      Array.of_list stack_frame_records
    in
    struct_set_body stack_frame_type stack_frame_records_arr false;
    let stack_frame_length =
      Array.length (struct_element_types stack_frame_type)
    in
    let isRoot = parent_stack_frame = None in
    let var_par_records =
      let initial_index = if isRoot then 0 else 1 in
      let par_records =
        let rec par_records_of_fparDefs index = function
          | [] -> []
          | (fpd : Ast.fparDef) :: tail ->
              let id = List.hd fpd.id_list in
              let isRef = fpd.ref in
              let isArray = fpd.fpar_type.array_dimensions <> [] in
              (id, index, isRef, isArray)
              :: par_records_of_fparDefs (index + 1) tail
        in
        par_records_of_fparDefs initial_index params_list
      in
      let initial_index = initial_index + List.length par_records in
      let var_records =
        let rec var_records_of_varDefs index = function
          | [] -> []
          | vd :: tail ->
              let id = List.hd vd.id_list in
              let isArray = vd.var_type.array_dimensions <> [] in
              (id, index, false, isArray)
              :: var_records_of_varDefs (index + 1) tail
        in
        var_records_of_varDefs initial_index vars_list
      in
      par_records @ var_records
    in
    funcDef.stack_frame <-
      Some
        {
          parent_stack_frame;
          stack_frame_type;
          access_link = access_link_opt;
          stack_frame_addr = None;
          var_records =
            (if isRoot then
               var_par_records
             else
               ("access_link", 0, false, false) :: var_par_records);
          stack_frame_length;
        }
  in
  set_stack_frame ();
  List.iter
    (fun ld -> match ld with L_funcDef fd -> set_stack_frames fd | _ -> ())
    funcDef.local_def_list

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
