(* tableau de cellules *)
open Cell
open Tree

let naive = ref false;;

let size = (20,10) (* lignes, colonnes *)

(* le tableau que l'on manipule dans le programme *)
(* tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell

let read_cell co = thesheet.(fst co).(snd co)

let update_cell_formula co f = thesheet.(fst co).(snd co).formula <- f
let update_cell_value co v = thesheet.(fst co).(snd co).value <- v


(* exécuter une fonction (f) sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done


(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst(Float 0.); dependancies = Nil } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()
;;



(********** calculer les valeurs à partir des formules *************)

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None"
 * N.B. : on pourrait être moins naïf *)
let invalidate_sheet () =
  let f i j = update_cell_value (i,j) None in
  sheet_iter f
;;

(* à faire : le cœur du programme *)
let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (p,q) -> begin
  	 match (eval_cell p q) with
  	| None -> failwith "eval_form: Unexpected None"
  	| Some(nb) -> nb
  end
  | Op(o,fs) -> begin
  	 match o with
  	| S -> List.fold_left add_number (Int 0) (List.map eval_form fs)
  	| M -> List.fold_left mult_number (Int 1) (List.map eval_form fs)
  	| A -> div_number (List.fold_left add_number (Int 0) (List.map eval_form fs)) (Int (List.length fs))
  	| MAX -> List.fold_left max_number (Int min_int) (List.map eval_form fs)
  end

(* ici un "and", car eval_formula et eval_cell sont a priori
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  let c = read_cell (i,j) in
  if c.value = None then
  	c.value <- Some(eval_form c.formula);
  c.value
;;


(* fonctions qui à partir d'une formule f renvoie la liste des coordonnées des cellules apparaissant dans f (dépendances antérieures) *)
let back_dependancies f =
  let l = ref [] in
  let rec aux = function
    | Cst(a) -> ()
    | Cell(a,b) -> l := (a,b)::(!l)
    | Op(o,fs) -> List.iter aux fs
  in aux f;
  !l
;;

(* fonction qui met à jour les dépendances sachant que la cellule de coordonnées co va recevoir la formule f *)
let update_back_dependancies co f =
  (* Printf.printf("%i %i: dependancies changed:\n") (fst co) (snd co); *)
  let c = read_cell co in
  let anciennes_dependances = back_dependancies c.formula in
  let nouvelles_dependances = back_dependancies f in
  let rec aux_supprime = function
    | [] -> ()
    | h::t -> let cellule = read_cell h in
      cellule.dependancies <- (delete co cellule.dependancies);
      (* Printf.printf("co supprimé dans %i %i\n") (fst h) (snd h); *)
      aux_supprime t
  in aux_supprime anciennes_dependances;
  let rec aux_ajoute = function
    | [] -> ()
    | h::t -> let cellule = read_cell h in
      cellule.dependancies <- (insert co cellule.dependancies);
      (*Printf.printf("co ajouté dans %i %i\n") (fst h) (snd h);*) (* pour le déboggage *)
      aux_ajoute t
  in aux_ajoute nouvelles_dependances
;;

(* fonction qui calcule les nouvelles valeurs des cellules qui dépendent (récursivement) de la cellule modifée *)
let update_up_dependancies co =
  let c = read_cell co in
  c.value <- None;
  let rec aux_None = function
    | Nil -> ()
    | Node(racine,_,fils_gauche,fils_droit) -> let cellule = read_cell racine in
      cellule.value <- None;
      aux_None fils_gauche;
      aux_None fils_droit
  in aux_None c.dependancies;
  (* on doit d'abord tout mettre à None, puis tout recalculer dans les dépendances au cas où les dépendances dépendent d'elles-même entre elles
  (il y a des valeurs out-of-date à éliminer) *)
  let _ = eval_cell (fst co) (snd co) in
  let rec aux_calcule = function
    | Nil -> ()
    | Node(racine,_,fils_gauche,fils_droit) ->  let _ = eval_cell (fst racine) (snd racine) in
      aux_calcule fils_gauche;
      aux_calcule fils_droit
  in aux_calcule c.dependancies
;;


let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter eval_cell
