{
  open Parser;;        (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)
  | eof             { EOF }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '='             { EQUAL }
  | ';'             { SEMICOL }
  | ':'             { COLON }
  | '.'             { DOT }
  | "Show" { SHOW }
  | "ShowAll" { SHOWALL }
  | "SUM" { SUM }
  | "MULT" { MULT }
  | "MAX" { MAX }
  | "AVERAGE" { AVERAGE }
  | ('-'|'+')?['0'-'9']+'.'['0'-'9']* as s { NBR (float_of_string s) }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['A'-'Z']+ as s { CELLROW s }
