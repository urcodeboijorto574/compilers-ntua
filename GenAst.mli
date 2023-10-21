open Llvm

val gen_func : Ast.funcDef -> unit
val the_module : Llvm.llmodule