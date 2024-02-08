open Llvm

(** [gen_on] takes the root node of the AST and generates the intermediate code
    representation. The tree traversal is executed by the DFS algorithm. *)
val gen_on : Ast.funcDef -> bool -> unit

(** [the_module] is a variable that stores all the elements of the program in
    LLVM IR code. *)
val the_module : Llvm.llmodule
