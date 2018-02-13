open Tree
open Numbers

(* p.ex. ("B",7) *)
type cellname = string*int


(* les deux fonctions ci-dessous sont a reprendre, un jour ou l'autre :
 * elles ne marchent que pour des noms de colonnes ne comportant qu'un
 * caractère *)

(* Dans la suite :
 * PAR CONVENTION : cn désigne un nom de cellule
 *                  co désigne des coordonnées de cellule *)

let cellname_to_coord cn =
  let column = ref 0 in
  if String.length (fst cn) > 1 then
  begin
    for i=0 to (String.length (fst cn) -1) do
      let lettre = int_of_char (fst cn).[i] - 65 in
      if !column = 0 then column := lettre
      else column := 26*(!column + 1) + lettre; (* lettres de 0 à 25 *)
    done
  end
  else column := int_of_char (fst cn).[0] - 65;
  (snd cn -1, !column)
;;

(* En itératif :
let coord_to_cellname co =
  let column_nbr = ref (snd co) in
  let name = ref "" in
  while !column_nbr != 0 do
    let lettre = string_of_int (!column_nbr mod 26 + 65) in
    name := (String.concat "" [lettre; !name]);
    column_nbr := !column_nbr/26;
  done;
  (!name, fst co + 1)
;;
*)

(* En récursif : *)
let coord_to_cellname co =
  let rec aux co2 name =
      if co2 = 0 then name
    else
      let lettre = string_of_int (co2 mod 26 + 65) in
      aux (co2/26) (String.concat "" [lettre; name])
  in let name = aux (snd co) "" in
  (name, fst co + 1)
;;

(* operations que l'on peut utiliser dans les formules *)
type oper = S | M | A | MAX | MIN | DIV | MOD | MINUS | INV | OPPOSITE | IFTE(* sum, multiply, average, max *)

(* formules : une valeur, la même valeur qu'une autre cellule, une opération et
 * ses arguments. On rajoute la possibilité de faire appel à une fonction. *)
type form = Cst of number | Cell of (int*int) | Op of oper * form list
                          | Fun of (int * form * form)

(* cellules *)
(* un type enregistrement
 * "mutable" signifie que l'on pourra modifier le champ
 * type 'a option = None | Some of 'a (ici, 'a c'est number) *)

(* On a rajouté un champ : dependancies
 * Lorsqu'on modifie une cellule wlog A1, il faut être capable de mettre à 'None' les cellules
 * qui en dépendent - ie les cellules pour lesquelles 'A1' apparaît dans la formule) *)

type cell = { mutable formula : form; mutable value : number option; mutable dependancies : (int * (int * int)) avlTree}
(* type des dépendaces: arbre AVL contenant: numéro de la feuille courante * coordonnées *)

(* par défaut, une cellule n'a pas de valeur, et la formule
   correspondante est la constante 0. *)
let default_cell = { formula = Cst(Int 0); value = None; dependancies = Nil}


(************ affichage **************)
let cell_name2string cn = (fst cn)^(string_of_int (snd cn))

let cell_val2string c = match c.value with
  | None -> "_"
  | Some n -> string_of_number n

let oper2string = function
(* Converti un opérateur en chaîne de caractères *)
  | S -> "SUM"
  | M -> "MULT"
  | A -> "AVERAGE"
  | MAX -> "MAX"
  | MIN -> "MIN"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | MINUS -> "MINUS"
  | INV -> "INV"
  | OPPOSITE -> "OPPOSITE"
  | IFTE -> "IF _ THEN _ ELSE _ "

let ps = print_string

let rec list2string f = function
(* Converti une liste en chaine de ce caractères *)
  | [x] -> f x
  | x::xs ->
     begin
       f x ^ ";" ^ list2string f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec show_list f = function
(* Affiche une liste *)
  | [x] -> f x
  | x::xs ->
     begin
       f x;
       ps";";
       show_list f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec form2string = function
(* Converti une formule en chaîne de caractères *)
  | Cell c -> cell_name2string (coord_to_cellname c)
  | Cst n -> string_of_number n
  | Op(o,fl) ->
     begin
       (oper2string o) ^ "(" ^ list2string form2string fl ^ ")"
     end
  | Fun(s,arg1,arg2) -> "s" ^ (string_of_int s) ^ "(" ^ form2string arg1 ^ ";" ^ form2string arg2 ^ ")"

let rec show_form f = ps (form2string f)
(* Affiche une formule *)
