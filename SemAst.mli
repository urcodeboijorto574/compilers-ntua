val sem_on : Ast.funcDef -> unit
(** [sem_on] takes the root node of the AST and does
    the semantic analysis of all the AST. The tree
    traversal is executed by the DFS algorithm. *)
