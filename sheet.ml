(* tableau de cellules *)
open Cell
open Tree

let naive = ref false;;
let feuille_courante = ref 1;;

let size = (100,100) (* lignes, colonnes *) (* Il fallait changer la taille pour autoriser des noms de colonne sur plusieurs lettres. *)

let out_of_bounds co =
  (0 > fst co) || (fst co >= 100) || (0 > snd co) || (snd co >= 100)
(* le tableau que l'on manipule dans le programme *)
(* tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *)

let sheets = Array.init 10 (fun _ -> Array.make_matrix (fst size) (snd size) default_cell);; (* On crée nos 10 feuilles de calcul vides *)

let read_cell co = sheets.(!feuille_courante).(fst co).(snd co)

let read_cell_from_sheet sheet co = sheets.(sheet).(fst co).(snd co)

let update_cell_formula co f = sheets.(!feuille_courante).(fst co).(snd co).formula <- f
let update_cell_value co v = sheets.(!feuille_courante).(fst co).(snd co).value <- v


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
let init_sheet k = (* il faut préciser la feuille de calcul à initialiser *)
  let init_cell i j =
    let c = { value = None; formula = Cst(Float 0.); dependancies = Nil } in
    sheets.(k).(i).(j) <- c
  in
  sheet_iter init_cell
;;

(* on y va, on initialise *)
for k = 0 to 9 do
  let _ = init_sheet k in ();
done
;;


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

let rec apply_to_up_dependancies fonction = function (* fonction d'ordre supérieure *)
  | Nil -> ()
  | Node(racine,_,fils_gauche,fils_droit) ->
    let cellule = read_cell_from_sheet (fst racine) (snd racine) in
    fonction racine;
    apply_to_up_dependancies fonction cellule.dependancies;
    apply_to_up_dependancies fonction fils_gauche;
    apply_to_up_dependancies fonction fils_droit
;;


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
    | MIN -> List.fold_left min_number (Int max_int) (List.map eval_form fs)
    | MINUS -> extract_value (List.fold_left minus_number None (List.map (fun f -> Some(eval_form f)) fs))
    | OPPOSITE -> extract_value (List.fold_left minus_number (Some(Int 0)) (List.map (fun f -> Some(eval_form f)) fs))
    | DIV -> extract_value (List.fold_left generalised_div_number None (List.map (fun f -> Some(eval_form f)) fs))
    | INV -> extract_value (List.fold_left generalised_div_number (Some(Float 1.)) (List.map (fun f -> Some(eval_form f)) fs))
    | MOD -> extract_value (List.fold_left mod_number None (List.map (fun f -> Some(eval_form f)) fs))
    | IFTE -> Int 0
  end
  | Fun(s,arg1,arg2) -> begin
      (* On calcule les arguments, puis on bascule sur la feuille s *)
      (* Les arguments sont alors placés dans A1 et A2 et le résultat va dans A3 *)
      let arg1_value = eval_form arg1
      and arg2_value = eval_form arg2
      and current_sheet = !feuille_courante in

      feuille_courante := s;

      let cell_a1 = read_cell (cellname_to_coord ("A", 1))
      and cell_a2 = read_cell (cellname_to_coord ("A", 2))
      and cell_a3 = read_cell (cellname_to_coord ("A", 3)) in

      apply_to_up_dependancies (fun racine -> let cellule = read_cell_from_sheet (fst racine) (snd racine) in
        cellule.value <- None) cell_a1.dependancies;
      apply_to_up_dependancies (fun racine -> let cellule = read_cell_from_sheet (fst racine) (snd racine) in
        cellule.value <- None) cell_a2.dependancies;

      cell_a1.value <- Some(arg1_value);
      cell_a2.value <- Some(arg2_value);

      let function_res = eval_form cell_a3.formula in
      (* Printf.printf ("Sheet:%s -- arg1:%s, arg2:%s, res:%s\n") (string_of_int s) (string_of_number arg1_value) (string_of_number arg2_value) (string_of_number function_res);
      Printf.printf ("Formula was : %s\n") (form2string cell_a3.formula); *)
      feuille_courante := current_sheet;
      function_res
  end

(* ici un "and", car eval_formula et eval_cell sont a priori
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  let c = read_cell (i,j) in
  if c.value = None then
  	c.value <- Some(eval_form c.formula);
  c.value
;;


(* Fonction qui à partir d'une formule f renvoie la liste des coordonnées
 * des cellules apparaissant dans f (dépendances antérieures).
 * Étant donné qu'il y a possiblement plusieurs feuilles de calcul et qu'on peut
 * faire des appels sur d'autres feuilles de calcul, on peut avoir des dépendances
 * cycliques sur plusieurs feuilles de calcul. Il faut donc retenir les coordonnées
 * plus le numéro de la feuille. *)
let back_dependancies f =
  let l = ref [] in
  let rec aux = function
    | Cst(a) -> ()
    | Cell(a,b) -> l := (!feuille_courante, (a,b))::(!l)
    | Op(o,fs) -> List.iter aux fs
    | Fun(i,arg1,arg2) -> (* C'est un peu moche mais on change la feuille courante pour mettre des dépendances. *)
        begin
          let backup = !feuille_courante in
          feuille_courante := i;
          let a3 = read_cell (cellname_to_coord ("A", 3)) in
          l := (i, cellname_to_coord ("A", 3)) :: (!l);
          aux a3.formula;
          feuille_courante := backup
        end
  in aux f;
  !l
;;

(* fonction qui met à jour les dépendances sachant que la cellule de coordonnées co va recevoir la formule f *)
let update_back_dependancies co f =
  let rec apply_to_dependancies fonction = function (* fonction d'ordre supérieure *)
    | [] -> ()
    | h::t -> let cellule = read_cell_from_sheet (fst h) (snd h) in (* snd h est un couple: les coordonnées *)
      cellule.dependancies <- (fonction (!feuille_courante, co) cellule.dependancies);
      apply_to_dependancies fonction t
  in
  let c = read_cell co in
  let anciennes_dependances = back_dependancies c.formula in
  let nouvelles_dependances = back_dependancies f in
  apply_to_dependancies delete anciennes_dependances;
  apply_to_dependancies insert nouvelles_dependances
;;

(* fonction qui calcule les nouvelles valeurs des cellules qui dépendent (récursivement) de la cellule modifée *)
let update_up_dependancies co =
  let c = read_cell co in
  c.value <- None;
  apply_to_up_dependancies (fun racine -> let cellule = read_cell_from_sheet (fst racine) (snd racine) in
    cellule.value <- None) c.dependancies;
  (* on doit d'abord tout mettre à None, puis tout recalculer dans les dépendances au cas où les dépendances dépendent d'elles-même entre elles
  (il y a des valeurs out-of-date à éliminer) *)
  let _ = eval_cell (fst co) (snd co) in
  apply_to_up_dependancies (fun racine2 -> let racine = snd racine2 in let _ = eval_cell (fst racine) (snd racine) in ()) c.dependancies
;;

(* Les cycles :
 * Avant de faire un calcul, on vérifie s'il n'y a pas de cycle.
 * Les cycles créent des stack-overflow : car avant de faire un calcul, on met les valeurs de toutes
 * les dépendances montantes à None de façon récursive. Or, s'il y a un cycle, on ne s'arrêtera jamais
 * de mettre des valeurs à None.
 * Même si on corrige cela, lors du calcul, on calcule une dépendance ssi sa valeur est à None. Donc, si
 * les dépendances sont cycliques, on fera une boucle infinie d'appels récursifs.

 * On suppose qu'on insère une nouvelle formule A1=f(..) dans la feuille de calcul.
 * On suppose que la feuille de calcul est initialement acyclique. Alors il suffit de vérifier que f(..)
 * ne nécessite pas la valeur de A1. Si on prend un cycle Ai ->* Ai, soit il passe par A1, ie on a
 * Ai ->* A1 ->* Ai donc A1 ->* Ai ->* A1 donc f(..) nécessite A1; soit il ne passe pas par A1, donc est
 * indépendant de f(..), ce qui est impossible car la feuille de calcul est supposée initialement acyclique. *)

let uncycling_formula co f =
  (** Regarde si la formule co=f(..) n'est pas cyclique, en supposant que la feuille de calcule n'est pas cyclique. *)
  let rec co_needed current_sheet = function
      (* La remarque précécente nous assure que cette fonction termine bien, et qu'il n'y a pas besoin de structure
       * de donnée pour retenir chaque cellule qu'on croise dans les formules *)
    | Cell c -> if (out_of_bounds co) || (c = co && current_sheet = !feuille_courante) then true else let c0 = read_cell c in co_needed current_sheet c0.formula
    | Cst _ -> false
    | Op (_, fl) -> List.fold_left (fun b -> fun l -> b || (co_needed current_sheet l)) false fl
    | Fun (s,arg1,arg2) -> let a3 = read_cell_from_sheet current_sheet (cellname_to_coord ("A", 3)) in
        (co_needed current_sheet arg1) || (co_needed current_sheet arg2) || co_needed s a3.formula
  in
  not (co_needed !feuille_courante f)
;;


let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter eval_cell
