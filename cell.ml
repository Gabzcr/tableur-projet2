open Tree

(* les nombres avec lesquels on calcule *)
type number = Float of float | Int of int
let print_number = function
  | Float(f) -> print_float f
  | Int(i) -> print_int i
;;

let string_of_number = function
  | Float(f) -> string_of_float f
  | Int(i) -> string_of_int i
;;

let add_number x y = match x,y with
  | Int(i1), Int(i2) -> Int(i1 + i2)
  | Float(f1), Int(i2) -> Float(f1 +. float_of_int i2)
  | Int(i1), Float(f2) -> Float(float_of_int i1 +. f2)
  | Float(f1), Float(f2) -> Float(f1 +. f2)
;;

let mult_number x y = match x,y with
  | Int(i1), Int(i2) -> Int(i1 * i2)
  | Float(f1), Int(i2) -> Float(f1 *. float_of_int i2)
  | Int(i1), Float(f2) -> Float(float_of_int i1 *. f2)
  | Float(f1), Float(f2) -> Float(f1 *. f2)
;;

let div_number x y = match x,y with (* pour celle là il faut imposer le résultat en flottant *)
  | Int(i1), Int(i2) -> Float(float_of_int i1 /. float_of_int i2)
  | Float(f1), Int(i2) -> Float(f1 /. float_of_int i2)
  | Int(i1), Float(f2) -> Float(float_of_int i1 /. f2)
  | Float(f1), Float(f2) -> Float(f1 /. f2)
;;

let max_number x y = match x,y with
  | Int(i1), Int(i2) -> if i1 > i2 then x else y
  | Float(f1), Int(i2) -> if f1 > float_of_int i2 then x else y
  | Int(i1), Float(f2) -> if float_of_int i1 > f2 then x else y
  | Float(f1), Float(f2) -> if f1 > f2 then x else y
;;

(* p.ex. ("B",7) *)
type cellname = string*int


(* les deux fonctions ci-dessous sont a reprendre, un jour ou l'autre :
 * elles ne marchent que pour des noms de colonnes ne comportant qu'un
 * caractère *)
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
type oper = S | M | A | MAX (* sum, multiply, average, max *)

(* formules : une valeur, la même valeur qu'une autre cellule, une opération et
 * ses arguments *)
type form = Cst of number | Cell of (int*int) | Op of oper * form list

(* cellules *)
(* un type enregistrement
 * "mutable" signifie que l'on pourra modifier le champ
 * type 'a option = None | Some of 'a (ici, 'a c'est number) *)

(* On a rajouté un champ : dependancies
 * Lorsqu'on modifie une cellule wlog A1, il faut être capable de mettre à 'None' les cellules
 * qui en dépendent - ie les cellules pour lesquelles 'A1' apparaît dans la formule) *)

type cell = { mutable formula : form; mutable value : number option; mutable dependancies : (int * int) avlTree}

(* par défaut, une cellule n'a pas de valeur, et la formule
   correspondante est la constante 0. *)
let default_cell = { formula = Cst(Int 0); value = None; dependancies = Nil}


(************ affichage **************)
let cell_name2string cn = (fst cn)^(string_of_int (snd cn))

let cell_val2string c = match c.value with
  | None -> "_"
  | Some n -> string_of_number n

let oper2string = function
  | S -> "SUM"
  | M -> "MULT"
  | A -> "AVERAGE"
  | MAX -> "MAX"

let ps = print_string

let rec list2string f = function
  | [x] -> f x
  | x::xs ->
     begin
       f x ^ ";" ^ list2string f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec show_list f = function
  | [x] -> f x
  | x::xs ->
     begin
       f x;
       ps";";
       show_list f xs
     end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec form2string = function
  | Cell c -> cell_name2string (coord_to_cellname c)
  | Cst n -> string_of_number n
  | Op(o,fl) ->
     begin
       (oper2string o) ^ "(" ^ list2string form2string fl ^ ")"
     end

let rec show_form f = ps (form2string f)
