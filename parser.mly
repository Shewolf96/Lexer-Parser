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
%token COLON COMMA SEMICOL
%token INT_T BOOL_T
%token <Int32.t> INT
%token <string>CHAR
%token IF ELSE WHILE
%token <Char.t>BINOP 
%token <string>RELOP 
%token <Char.t>UNOP
%token ASSIGN
%token RET
%token <string>STRING
%token UNDERSCORE

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
    | i = identifier ; LEFT_PAR ; var_list = var_declarations ; RIGHT_PAR ; ret = type_list ; body = body_option
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



type_list:
    |
    { [] }
    | COLON ; l = separated_list (COMMA, type_expression)
    { l }



body_option:
  | body = statement_block
  { Some body }
  |
  { None }

statement_block:
  | LEFT_BRA ; statement_list = statement_list ; RIGHT_BRA
  { STMTBlock 
      { loc = mkLocation $startpos
      ; body = statement_list
      }
  }
  
statement_list:
    | l = rev_statement_list
    { List.rev l }

rev_statement_list:
  | tail = rev_statement_list ; x = statement
  { x::tail }
  |
  { [] } (* statement list - trzeba gdzies tam sprawdzac czy return byl na koncu *)

(*________________STATEMENT__vvvv________________________*)

statement:
    | c = call
    { STMT_Call c }

    | id = lvalue_id ; ASSIGN ; expr = expression
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

    | var_list = var_decl_opt ; ASSIGN ; fcall = call
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


lvalue_id:
    | id = identifier 
    { LVALUE_Id
        { loc =mkLocation $startpos
        ; id = id
        }
    }


call:
    | id = identifier ; LEFT_PAR ; args = expression_list ; RIGHT_PAR
    { Call 
        { tag = mkTag ()
        ; loc = mkLocation $startpos
        ; callee = id
        ; arguments = args
        }
    }

var_decl_opt:
    | tail = var_decl_opt ; COMMA ; x = var_declaration
    { (Some x)::tail }
    | tail = var_decl_opt ; COMMA ; UNDERSCORE
    { (None)::tail }
    | 
    { [] }


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
      ; value = c.[1]
      }
  }  

  | s = string_expr
  { EXPR_String
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; value = s
      }
  }

  | e1 = expression ; op = relop ; e2 = expression
  { EXPR_Relation
      { tag = mkTag ()
      ; loc = mkLocation $startpos
      ; op = op
      ; lhs = e1
      ; rhs = e2
      }
  }

relop:
    | RELOP 
    { RELOP_Eq }
    (*{ match $1 with
      | "xd" }*)


else_option:
    | ELSE ; s = statement
    { Some s }
    | 
    { None }

var_decl_assing:
  | ASSIGN ; e = expression
  { Some e }
  | 
  { None }

expression_list:
    | l = separated_list(COMMA, expression)
    { l }



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
  | INT_T
  {
    TEXPR_Int
    { loc = mkLocation $startpos }
  }
  | BOOL_T 
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
  | e = expression
  { Some e }
  |
  { None }



string_expr:
  | STRING
  { $1 }

char_expr:
  | CHAR
  { $1 }

int_expr: 
  | INT
  { $1 }

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
