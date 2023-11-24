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

let lib_function_names = ["writeInteger"; "writeString"; "writeByte"]

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

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)

(* TODO: t_type may be an array type*)
(* let create_entry_block_alloca the_function var_name t_type =
   let builder = builder_at context (instr_begin (entry_block the_function)) in
   build_alloca t_type var_name builder *)

and gen_header (header : Ast.header) access_link =
  let name = header.id in
  let args = expand_fpar_def_list header.fpar_def_list in
  let args_array = Array.of_list args in
  Hashtbl.add named_functions (Hashtbl.hash name) args;
  let ret_type = header.ret_type in
  let access_link_list =
    match access_link with Some x -> [ x ] | None -> (* Printf.printf("emptu"); *) []
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
  (* Printf.printf("we cool\n");
  (* Set names for all arguments. *)
  Printf.printf "%d\n" (Array.length args_array); 
  Printf.printf "%d\n" (Array.length (params f)); 
  Printf.printf "%s\n" name; *)

  (* Array.iteri
    (fun i a ->
      if Array.length args_array > 0 then 
      begin
        let n =
          match args_array.(i).id_list with
          | [ id ] -> id
          (* will never reach here, becaues id_list has certainly only one element *)
          | _ -> failwith "error in list"
        in
        (* Set the name of each argument which is an llvalue, to a string *)
        set_value_name n a;
        Hashtbl.add named_values n a
      end
    )
    (params f); *)
  f (* why is this 'f' alone here? *)
    
(* Create array of parameters. This array is of like:
   [llvm int, llvm char, llvm int *, ...]*)
and gen_header_lib (header : Ast.header) =
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

(* Create an alloca for each argument and register the argument in the symbol
   * table so that references to it will succeed. *)
and create_argument_allocas the_function func_def stack_frame_alloca =
  let args = expand_fpar_def_list func_def.header.fpar_def_list in
  let args_array = Array.of_list args in
  let access_link_list =
    match func_def.access_link with Some al -> [ al ] | None -> []
  in
  let param_types_list = access_link_list @ List.map llvm_type_of_param args in
  let param_types_array = Array.of_list param_types_list in
  (* The problem here is that in params there is also the access link but in fpar def list it does not exist. So in fact params contain one more element than args_array*)
  let params_length = Array.length (params the_function) in
  let params_records = ref [] in
  Array.iteri
    (fun i ai ->
      let position =
        build_struct_gep stack_frame_alloca i "stack_frame elem" builder
      in
      (* i = 0 corresponds to the access link*)
      if i = 0 then (
        match access_link_list with
        (* funcion is main *)
        | [] -> ()
        | list -> 
          set_value_name "access_link" position;
          params_records := ("access_link", i) :: !params_records;
          ignore (build_store ai position builder))
      else
        let ith_param = args_array.(i - 1) in
        let var_name =
          match ith_param.id_list with
          | [ id ] -> id
          | _ -> failwith "error in list"
        in
        set_value_name var_name position;
        params_records := (var_name, i) :: !params_records;
        ignore (build_store ai position builder)
    )
  (params the_function);
  func_def.var_records <- !params_records


and gen_expr is_param_ref expr access_link_ptr stack_frame_alloca
    stack_frame_length funcDef =
  (* let the_stack_frame = build_load stack_frame_ptr "stack_Frame" builder in *)
  match expr with
  | E_const_int x -> const_int int_type x
  | E_const_char x -> const_int char_type (int_of_char x)
  | E_lvalue lv -> (
      match lv with
      | L_id id ->
          let lv_address = 
            let rec iterate i stack_frame funcDef =
              Printf.printf "index: %d\n%!" i;
              Printf.printf "stack frame length: %d\n%!" stack_frame_length;
              Printf.printf "var records length: %d\n%!" (List.length funcDef.var_records);

              let tuple = List.nth funcDef.var_records i in
              let var_name = fst tuple in
              Printf.printf "%s\n%!" var_name;
              if var_name = id then
                build_struct_gep stack_frame i "address" builder
              else if i + 1 < stack_frame_length then
                iterate (i + 1) stack_frame funcDef
              else
                match funcDef.parent_func with
                | Some p -> 
                  let access_link =
                    match access_link_ptr with
                    | None ->
                      failwith "fail. variable not found. sem analysis error"
                    | Some x -> x
                  in
                  iterate 0 access_link p
                | None -> failwith "fail variable not found"
            in
            iterate 0 stack_frame_alloca funcDef
          in
          if is_param_ref = false then
            build_load lv_address id builder
          else
            lv_address
      | L_comp (lv2, expr2) -> failwith "todo" (* TODO *)
      | _ -> failwith "tododd")
  | E_func_call fc ->
      (* Printf.printf "%s\n" fc.id; *)
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
            res :=
              gen_expr x.ref ith_elem access_link_ptr stack_frame_alloca
                stack_frame_length funcDef
              :: !res;
            incr i)
          fpar_def_list
      in
      (*List.iter(fun x -> Printf.printf("%d\n", x) ) !res;*)
      let rev_list = List.rev !res in
      (* let access_link_ptr_list =
        match access_link_ptr with Some x -> [ x ] | None -> []
      in
      let args_array = Array.of_list (access_link_ptr_list @ rev_list) in
      build_call callee args_array "calltmp" builder *)
      let args_array =
        if (List.mem fc.id lib_function_names) = false then Array.of_list ([stack_frame_alloca] @ rev_list) 
        else Array.of_list rev_list
      in
      build_call callee args_array "" builder
  | E_sgn_expr (sign, expr) -> (
      match sign with
      | O_plus ->
          gen_expr false expr access_link_ptr stack_frame_alloca
            stack_frame_length funcDef
      | O_minus ->
          build_neg
            (gen_expr false expr access_link_ptr stack_frame_alloca
               stack_frame_length funcDef)
            "minus" builder)
  | E_op_expr_expr (lhs, oper, rhs) -> (
      let lhs_val =
        gen_expr false lhs access_link_ptr stack_frame_alloca stack_frame_length funcDef
      in
      let rhs_val =
        gen_expr false rhs access_link_ptr stack_frame_alloca stack_frame_length funcDef
      in
      match oper with
      | O_plus -> build_add lhs_val rhs_val "addtmp" builder
      | O_minus -> build_sub lhs_val rhs_val "subtmp" builder
      | O_mul -> build_mul lhs_val rhs_val "multmp" builder
      | O_div -> build_sdiv lhs_val rhs_val "divtmp" builder
      | O_mod -> build_srem lhs_val rhs_val "modtmp" builder)
  | E_expr_parenthesized expr ->
      gen_expr false expr access_link_ptr stack_frame_alloca stack_frame_length funcDef

and gen_stmt stmt access_link_ptr stack_frame_alloca stack_frame_length funcDef =
  match stmt with
  | S_assignment (lv, expr) -> (
      match lv with
      | L_id id ->
        Printf.printf("segfault1\n%!");
          let lv_address = 
            let rec iterate i stack_frame funcDef =
              Printf.printf "index: %d\n%!" i;
              Printf.printf "stack frame length: %d\n%!" stack_frame_length;
              Printf.printf "%s var records length: %d\n%!" funcDef.header.id (List.length funcDef.var_records);
              let tuple = List.nth funcDef.var_records i in
              let var_name = fst tuple in
              Printf.printf "%s\n%!" var_name;
              if var_name = id then
                build_struct_gep stack_frame i "address_found" builder
              else if i + 1 < stack_frame_length then
                iterate (i + 1) stack_frame funcDef
              else
                match funcDef.parent_func with
                | Some p -> 
                  Printf.printf("got in here\n%!");

                  let access_link =
                    match access_link_ptr with
                    | None ->
                      failwith "fail. variable not found. sem analysis error"
                    | Some x -> x
                  in
                  iterate 0 access_link p
                | None -> failwith "fail variable not found"

            in
            iterate 0 stack_frame_alloca funcDef
          in
          let lv_value =
            gen_expr false expr access_link_ptr stack_frame_alloca
              stack_frame_length funcDef
          in
          ignore (build_store lv_value lv_address builder)
      | _ -> failwith "tododd")
  | S_func_call fc ->
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
            res :=
              gen_expr x.ref ith_elem access_link_ptr stack_frame_alloca
                stack_frame_length funcDef
              :: !res;
            incr i)
          fpar_def_list
      in
      let rev_list = List.rev !res in
      (* let stack_frame_alloca_list =
        match stack_frame_alloca with 
        | Some x -> Printf.printf "%s" fc.id; [ x ] 
        | None -> []
      in *)
      let args_array =
        if (List.mem fc.id lib_function_names) = false then Array.of_list ([stack_frame_alloca] @ rev_list) 
        else Array.of_list rev_list
      in
      ignore (build_call callee args_array "" builder)
      

  | S_return expr -> (
      match expr with
      | None -> ignore (build_ret_void builder)
      | Some e ->
          let ll_expr =
            gen_expr false e access_link_ptr stack_frame_alloca
              stack_frame_length funcDef
          in
          ignore (build_ret ll_expr builder))
  | _ -> failwith "todoaa"

and gen_funcDef func_def =
  let access_link_ptr =
    match func_def.parent_func with
    | Some p -> p.stack_frame_addr
    | None -> None
  in

  let the_func_ll = gen_header func_def.header func_def.access_link in
  let bb = append_block context "entry" the_func_ll in
  position_at_end bb builder;
  blocks_list := bb :: !blocks_list;
  let stack_frame =
    match func_def.stack_frame with
    | Some sf -> sf
    | None -> failwith "Fail. Stack frame for function not set."
  in
  let frame_array = struct_element_types stack_frame in
  let stack_frame_length = Array.length frame_array in
  let stack_frame_alloca = build_alloca stack_frame ("stack_frame_" ^ func_def.header.id) builder in
  ignore (create_argument_allocas the_func_ll func_def stack_frame_alloca);
  func_def.stack_frame_addr <- Some stack_frame_alloca;
  let params_length = Array.length (params the_func_ll) in
  let struct_index = ref (params_length) in
  Printf.printf "length of %s : %d\n%!" func_def.header.id params_length;
  let local_var_records = ref [] in
  (* iterate functions in dfs order *)
  let iterate local_def =
    match local_def with
    | L_varDef v ->
        let ll_type =
          llvm_type_of_t_type (Types.t_type_of_dataType v.var_type.data_type)
        in
        List.iter
          (fun x ->
            let position =
              build_struct_gep stack_frame_alloca !struct_index
                x builder
            in
            set_value_name x position;
            local_var_records := (x, !struct_index) :: !local_var_records;
            struct_index := !struct_index + 1)
          v.id_list;
        func_def.var_records <- (func_def.var_records @ !local_var_records);

    | L_funcDef fd -> gen_funcDef fd
    | L_funcDecl fdl -> failwith "todo" (* TODO: Function Declarations *)
  in
  List.iter iterate func_def.local_def_list;
  let list_length = List.length func_def.var_records in
  (* let pointer_to_me = stack_frame_alloca in *)
  let stmt_list = match func_def.block with Block b -> b in
  let stack_frame_alloca = param the_func_ll 0 in
  List.iter
    (fun stmt ->
      gen_stmt stmt access_link_ptr stack_frame_alloca stack_frame_length func_def)
    stmt_list;

    if block_terminator @@ insertion_block builder = None then
    ignore (build_ret_void builder);
  blocks_list :=
    List.tl !blocks_list (* if blocks_list = [] this will throw Failure _ *);
  if !blocks_list <> [] then
    position_at_end (List.hd !blocks_list) builder


let define_lib_funcs =
  let define_lib_func (fp_def_list, f_id, f_rtype) =
    let fun_header = newHeader (f_id, fp_def_list, f_rtype) in
    ignore (gen_header_lib fun_header)
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
  List.iter traverse_dfs fd.local_def_list;

(* take a func definition and do the following
   - create the stack frame containing the access link, parameters and local variables *)
and set_stack_frame funcDef =
  (* gather local var definitions in a list for funcDef *)
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
  let params_list = expand_fpar_def_list funcDef.header.fpar_def_list in
  let param_types_list = List.map llvm_type_of_param params_list in

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
  let stack_frame_records = access_link @ param_types_list @ var_types_list in
  let stack_frame_records_arr = Array.of_list stack_frame_records in
  struct_set_body stack_frame_ll stack_frame_records_arr false

and set_stack_frames funcDef =
  set_func_parents funcDef;
  (* match funcDef.access_link with
  | Some x -> Printf.printf("some")
  | None -> Printf.printf("None"); *)
  set_stack_frame funcDef;
  (* match funcDef.access_link with
  | Some x -> Printf.printf("some")
  | None -> Printf.printf("None"); *)
  let rec iterate local_def =
    match local_def with
    | L_funcDef fd ->
        set_stack_frame fd;
        List.iter iterate fd.local_def_list
    | _ -> ()
  in
  List.iter iterate funcDef.local_def_list;
  (* match funcDef.access_link with
  | Some x -> Printf.printf("some")
  | None -> Printf.printf("None"); *)
  

and gen_on asts =
  (* Printf.printf("we cool\n"); *)

  define_lib_funcs;
  set_stack_frames asts;

  gen_funcDef asts
