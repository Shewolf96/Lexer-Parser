{
  open Xi_lib
  open Parser
  open Parser_utils

  (* Lexing z biblioteki standardowej ocamla *)
  open Lexing

  (* Standardowo w YACC-podobnych narzędziach  to lekser jest uzależniony od parsera. To znaczy, że typ 
   * danych z tokenami definiuje moduł wygenerowany na bazie grammar.mly. Definiujemy alias na typ
   * tokenu na potrzeby interfejsów Xi_lib.Iface *)
  type token = Parser.token

  (* Obsługa błędu *)
  let handleError pos token =
      let exc = InvalidToken (mkLocation pos, token) in
      raise exc
      
  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   * Miejsce na twój kod w Ocamlu
   *)


  (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ----------------------------------------------------------------------------- *)

  }
  
  (* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   * Miejsce na nazwane wyrażenia regularne
   *)

  let whitespace = [' ' '\t']+
  let relop = "<=" | ">=" | '<' | '>' | "==" | "!="
  let unop = '-' | '!'
  let digit = ['0' - '9']
  let number = digit+
  let bool = "true" | "false"
  let identifier    = ['a'-'z' '_' 'A' - 'Z']['_' 'A' - 'Z' 'a'-'z' '0'-'9']*

  
  (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     ----------------------------------------------------------------------------- *)


  rule token = parse
      (* Trzeba pamiętać aby uaktualnić pozycje w lexbuf, gdy widzimy znak końca wiersza.
       * To się samo nie robi. Moduł Lexing z standardowej biblioteki daje do tego wygodną
       * funkcję new_line.
       *)
      | ['\n']
      { new_line lexbuf; token lexbuf }

      (* widzimy początek komentarza i przechodzimy do pomocniczego stanu *)
      | "//"
      { line_comment lexbuf }

      | eof { EOF }

      | whitespace { token lexbuf }

      | '"' { read_string (Buffer.create 0) lexbuf }

      | "int" { INT_T }
      | "bool" { BOOL_T }
      | "while" { WHILE }
      | "if" { IF }
      | "else" { ELSE }
      | "return" { RET }
      | "length" { LEN }


      | relop as rop
      { RELOP rop }

      | '+' { PLUS }
      | '-' { MINUS }
      | '/' { DIV }
      | '&' { AND }
      | '|' { OR }
      | '%' { MOD }
      | '*' { MULT }


(* hmmm  gdzies tu jakis stan pomocniczy bo trzeba odroznic -x od x - y :| *)
      | unop as uop 
      { UNOP uop }

      | "_" { UNDERSCORE }

      | "=" { ASSIGN }

      | "[" { LEFT_SQUARE }
      | "]" { RIGHT_SQUARE }

      | "{" { LEFT_BRA}
      | "}" { RIGHT_BRA}

      | "(" { LEFT_PAR }
      | ")" { RIGHT_PAR }

      | ":" { COLON }
      | ";" { SEMICOL }
      | "," { COMMA }

      | bool as t
      { BOOL ( bool_of_string t) }

      | "'" { read_char lexbuf }
      
      | number as num
      { INT (Int32.of_string num) }
      
      | identifier as id
      { IDENTIFIER id }


      (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         ----------------------------------------------------------------------------- *)
  
      | _
      { handleError (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme lexbuf) }

  (* Pomocniczy stan aby wygodnie i prawidłowo obsłużyć komentarze *)

  and read_string buf = parse
      | '"'       { STRING (Buffer.contents buf) }
      | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
      | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
      | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
      | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
      | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
      | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
      | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
      | '\\' '\"'  { Buffer.add_char buf '\"'; read_string buf lexbuf }
      | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
          read_string buf lexbuf
        }

  and read_char = parse
      | '\\' '/' "'" { CHAR '/'}
      | '\\' '\\' "'" { CHAR '\\'}
      | '\\' 'b' "'" { CHAR '\b' }
      | '\\' 'f' "'" { CHAR '\012' }
      | '\\' 'n' "'" { CHAR '\n' }
      | '\\' 'r' "'" { CHAR '\r' }
      | '\\' 't' "'" { CHAR '\t' }
      | '\\' '\"' "'" { CHAR '\"' }
      | [^ '"' '\\'] as c "'" { CHAR c }

  and line_comment = parse
      | '\n' 
      { new_line lexbuf; token lexbuf }

      (* Niektóre edytory nie wstawiają znaku końca wiersza w ostatniej linijce, jesteśmy
       * przygotowani na obsługę takiego komentarza.
       *)
      | eof
      { EOF }

      | _ 
      { line_comment lexbuf }
