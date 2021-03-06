%{
(* --- préambule: ici du code Caml --- *)

open Numbers
open Cell
open Command


let intervalle2list f1 f2 = match (f1, f2) with
  | (Cell (co1), Cell (co2) :: t) ->
  begin
    let l = ref t in
    let x1 = min (fst co1) (fst co2)
    and x2 = max (fst co1) (fst co2)
    and y1 = min (snd co1) (snd co2)
    and y2 = max (snd co1) (snd co2) in
    for i=x1 to x2 do
      for j=y1 to y2 do
        l := Cell(i,j) :: !l
      done;
    done;
    !l
  end
  | (_, _) -> failwith "Type 'Cell' attendu"
;;



%}
/* énumération des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <float> NBR       /* le lexème NBR a un attribut float */
%token <string> CELLROW       /* le lexème CELLROW a un attribut, de type string */
%token <string> SHEET       /* le s avant le numéro de la feuille de calcul */
%token LPAREN RPAREN EQUAL SEMICOL DOT COLON
%token SUM MULT AVERAGE MAX SHOW SHOWALL SWITCHTO MIN DIV MOD MINUS INV OPPOSITE IFTE
%token EOF

  /*
%start singlecomm
%type <Command.comm> singlecomm
    */

%start debut
%type <Command.comm list> debut

%%
debut:
   | clist EOF { $1 }
  ;

clist:
   | singlecomm clist { $1::$2 }
   | singlecomm                { [$1] }
  ;

  singlecomm:
   | cell EQUAL formula { Upd($1,$3) }
   | SHOW cell { Show($2) }
   | SHOWALL { ShowAll }
   | SWITCHTO tableau { SwitchTo($2) }
  ;

  cell:
   | CELLROW INT { ($1,$2) }
  ;

  tableau:
   | SHEET INT { ($1,$2) }
  ;

  t_function:
   | SHEET INT { ($2) }
  ;

  ifte:
   | IFTE { IFTE }
  ;

  operand:
   | SUM { S }
   | MULT { M }
   | AVERAGE { A }
   | MAX { MAX }
   | MIN { MIN }
   | MINUS { MINUS }
   | DIV { DIV }
   | MOD { MOD }
   | INV { INV }
   | OPPOSITE { OPPOSITE }
  ;

  formula:
   | NBR { Cst(Float $1) }
   | INT { Cst( Int $1) }
   | cell { Cell (Cell.cellname_to_coord $1) }
   | t_function LPAREN formula SEMICOL formula RPAREN { Fun($1,$3,$5) }
   | operand LPAREN forlist RPAREN { Op($1,$3) }
   | ifte LPAREN triplet RPAREN { Op($1,$3) }

  ;

  forlist:
   | formula { [$1] }
   | formula SEMICOL forlist { $1::$3 }
   | formula COLON forlist { intervalle2list $1 $3 }
  ;

  triplet:
   | formula SEMICOL formula SEMICOL formula { [$1;$3;$5] }
