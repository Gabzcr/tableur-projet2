{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | eof             { EOF }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '='             { EQUAL }
  | ';'             { SEMICOL }
  | ':'             { COLON }
  | '.'             { DOT }
  | "Show" { SHOW }
  | "ShowAll" { SHOWALL }
  | "SwitchTo" { SWITCHTO }
  | "SUM" { SUM }
  | "MULT" { MULT }
  | "MAX" { MAX }
  | "AVERAGE" { AVERAGE }
  | "MIN" { MIN }
  | "DIV" { DIV }
  | "MOD" { MOD }
  | "MINUS" { MINUS }
  | "INV" { INV }
  | "OPPOSITE" { OPPOSITE }
  | "IFTE" { IFTE }
  | ('-'|'+')?['0'-'9']+'.'['0'-'9']* as s { NBR (float_of_string s) }
  | ('-'|'+')?['0'-'9']+ as s { INT (int_of_string s) }
  | ['A'-'Z']+ as s { CELLROW s }
  | 's'+       as s { SHEET s }
