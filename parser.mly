(*
 * Menhir wygeneruje funkcję o nazwie file 
 *)
%start <Xi_lib.Ast.module_definition> file

%{
open Xi_lib
open Ast
open Parser_utils

(* Generator znaczników *)
let mkTag =
    let i = ref 0 in
    fun () ->
        let tag = !i in
        incr i;
        NodeTag tag

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na twój kod w Ocamlu
 *)

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)

%}

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na dyrektywy
 *)

%token EOF
%token <string>IDENTIFIER
%token LEFT_PAR RIGHT_PAR LEFT_SQUARE RIGHT_SQUARE LEFT_BRA RIGHT_BRA
%token COLON COMMA
%token INT BOOL
%token <int32.t> INT
%token <char.t> CHAR
%token IF ELSE WHILE
%token <char.t>BINOP <string>RELOP <char.t>UNOP
%token ASSIGN
%token WHILE IF ELSE
%token RET


(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)

%%

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na gramatykę
 *)


file:
    | x = rev_global_declarations ; EOF
    { ModuleDefinition {global_declarations= List.rev x } }

rev_global_declarations:
  | x = global_declaration
  { [ x ] }
  | tail = rev_global_declarations; x = global_declaration
  { x::tail }
  | 
  { [] }

global_declaration:
    | i = identifier ; LEFT_PAR ; var_list = var_declarations ; RIGHT_PAR ; ret = return_option ; body = body_option
    { GDECL_Function 
        { loc = mkLocation $startpos
        ; id = i
        ; formal_parameters = var_list
        ; return_types = ret
        ; body = body
        } 
    }

(*____________________v_________________________*)
(*____________________v_________________________*)

return_option:
    | COLON ; ret_types = List.rev rev_type_list
    { Some ret_types }
    |
    { None }

rev_type_list:
    | tail = rev_type_list ; COMA ; x = type_expression (*separated_list(COMMA, type_expression)*)
    { x::tail }
    | x = type_expression
    { [x] }
    |
    { [] }


body_option:
  | body = statement_block
  { Some body }
  |
  { None }

statement_block:
  | LEFT_BRA ; statement_list = List.rev rev_statement_list ; RIGHT_BRA
  { STMTBlock 
      { loc = mkLocation $startpos
      ; body = statement_list
      }
  }
  
rev_statement_list:
  | tail = rev_statement_list ; x = statement
  { x::tail }
  |
  { [] } (* statement list - trzeba gdzies tam sprawdzac czy return byl na koncu *)

(*________________STATEMENT__vvvv________________________*)

statement:
  | IDENTIFIER as id ; LEFT_PAR ; args = expression_list ; RIGHT_PAR
  { Call 
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; callee = id
      ; arguments = args
      }
  }

  | c = call
  { c }

(* tak bym chciala - to moze dzialac? 
and call:
  | IDENTIFIER as id ; LEFT_PAR ; args = expression_list ; RIGHT_PAR
  { Call 
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; callee = id
      ; arguments = args
      }
  } *)

  | IDENTIFIER as id ; ASSIGN ; expr = expression
  { STMT_Assign
      { loc = mkLocation $startpos
      ; lhs = id
      ; rhs = expr
      }
  }

  | var_decl = var_declaration ; init = var_decl_assing
  { STMT_VarDecl
      { var = var_decl
      ; init = init
      } 
  }

  | IF ; c = expression ; e1 = statement ; e2 = else_option ; 
  { STMT_If
      { loc = mkLocation $startpos
      ; cond = c
      ; then_branch = e1
      ; else_branch = e2
      }
  }

  | WHILE ; c = expression ; s = statement
  { STMT_While
      { loc = mkLocation $startpos
      ; cond = c
      ; body = s
      }
  }

  | RET ; expr_list = expression_list
  { STMT_Return
      { loc = mkLocation $startpos
      ; values = expr_list
      }
  }

  | var_list = var_declarations_opt ; ASSIGN ; fcall = fun_call (*napisac raz i uzyc dla STMT_Call tez*)
  { STMT_MultiVarDecl
      { loc = mkLocation $startpos
      ; vars = var_list
      ; init = fcall
      }
  }

  | st_block = statement_block
  { STMT_Block
    st_block }



(*______________STATEMENT ^^^^_________________________*)


var_declarations_opt:
  | [] 
  { None }
  | var_list 
  { Some var_list }

fun_call: 

expression:

  | id = identifier
  { EXPR_Id
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; id = id
      }
  }

  | n = int_expr
  { EXPR_Int
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; value = n
      }
  }

  | c = char_expr
  { EXPR_Char 
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; value = c
      }
  }  

  | s = string_expr
  { EXPR_String
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; value = s
      }
  }

  | e1 = expression ; RELOP as op ; e2 = expression
  { EXPR_Relation
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; op = op
      ; lhs = e1
      ; rhs = e2
      }
  }




else_option: ...

var_decl_assing:
  | ASSIGN ; e = expression
  { Some e }
  | 
  { None }

expression_list: ...



(*_________________________^_____________________*)


var_declarations: 
  | var_list = separated_list(COMMA, var_declaration)
    {var_list}

var_declaration:
  | i = identifier ; COLON ; t = type_expression
  { VarDecl
    { loc = mkLocation $startpos
    ; id = i
    ; tp = t
    }
  }

type_expression:
  | INT 
  {
    TEXPR_Int
    { loc = mkLocation $startpos }
  }
  | BOOL 
  {
    TEXPR_Bool
    { loc = mkLocation $startpos }
  }
  | t = type_expression ; LEFT_SQUARE ; size = dim_size ; RIGHT_SQUARE
  {
    TEXPR_Array
    { loc = mkLocation $startpos
    ; sub = t
    ; dim = size
    }
  }

dim_size:
  | num = int_expr
  { Some num }
  |
  { None }



string_expr:
  | STRING
  { string $1}

char_expr:
  | CHAR
  { char.t $1 }

int_expr: 
  | INT
  { int32.t $1 } (*????*)

identifier:
    | IDENTIFIER
    { Identifier $1 }



(* 
   ** przykład użycia mkLocation 

    use_declaration:
        | USE suffix(identifier, opt(SEMICOLON))
        { GDECL_Use {loc=mkLocation $startpos; id=$2} }

   ** przykład użycia mkTag

    atomic_expression:
        | identifier
        { EXPR_Id {loc=mkLocation $startpos; id=$1; tag=mkTag ()} }
*)

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)
