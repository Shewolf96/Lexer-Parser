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
%token LEFT_PAR RIGHT_PAR COLON COMMA
%token INT BOOL

(* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   ----------------------------------------------------------------------------- *)

%%

(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
 * Miejsce na gramatykę
 *)


(* Obecnie potrafimy sparsować tylko pusty plik (wymagamy od razu tokena EOF) *)
file:
    | x = rev_global_declarations ; EOF
    { ModuleDefinition {global_declarations= List.rev x } }

rev_global_declarations:
  | x = global_declaration
  { [ x ] }
  | tail = rev_global_declarations; x = global_declaration
  { x::tail }
  | 
  {[]}

global_declaration:
    | i = identifier ; LEFT_PAR ; var_list = var_declarations ; RIGHT_PAR (* ; ret = return_option; body = body_option  *)
    { GDECL_Function 
        { loc = mkLocation $startpos
        ; id = i
        ; formal_parameters = var_list
        ; return_types = [] (*ret*)
        ; body = None
        } 
    }

var_declarations: 
  | list = separated_list(COMMA, var_declaration)
    {list}

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
