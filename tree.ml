(************* Un fichier pour manipuler des arbres **************)

(* AVL trees
 * Les AVL trees sont des arbres binaires de recherche tels que
 * la diff√©rence de hauteur entre leur fils gauche et fils droit est
 * au plus 1.
 * Cela assure que si n est le nombre de noeuds, alors la hauteur h v√©rifie
 * log n <= h <= 3/2 log n
 * On aura toujours un temps logarithmique sur nos op√©rations classiques .*)

(* Remarque : 
 * Nos AVL trees sont ici SANS DOUBLON. 
 * Cela para√Æt en effet appropri√© pour l'utilisation qu'on en fait. *)

type 'a avlTree = Nil | Node of 'a * int * 'a avlTree * 'a avlTree

let height = function
  | Nil -> 0
  | Node (_, h, _, _) -> h

(* Les fonctions de rotation sont essentielles
 * Ce sont elles qui permettent d'√©quilibrer l'arbre *)

let rotate_right = function
  | Node (x, h, Node(fg_x, fg_h, fg_gauche, fg_droit), fd) ->
     Node (fg_x, 1 + max (height fg_gauche) (1 + max (height fg_droit) (height fd)), fg_gauche, Node (x, 1 + max (height fg_droit) (height fd), fg_droit, fd))
  | _ -> failwith "Rotation droite impossible"

let rotate_left = function
  | Node (x, h, fg, Node (fd_x, fd_h, fd_gauche, fd_droit)) ->
     Node (fd_x, 1 + max (height fd_droit) (1 + max (height fg) (height fd_gauche)), Node (x, 1 + max (height fg) (height fd_gauche), fg, fd_gauche), fd_droit)
  | _ -> failwith "Rotation gauche impossible"

let left_tree = function
  | Node (_, _, fg, _) -> fg
  | _ -> failwith "Pas d'arbre gauche"

let right_tree = function
  | Node (_, _, _, fd) -> fd
  | _ -> failwith "Pas d'arbre droit"

let equilibrate a = match a with
  | Nil -> Nil
  | Node (_, _, fg, fd) -> let hg = height fg
			  and hd = height fd in
			  if (hg + 1) > hd then rotate_right a
			  else if (hd + 1) > hg then rotate_left a
			  else a

let rec insert x a = match a with
  | Nil -> Node (x, 1, Nil, Nil)
  | Node (y, h, fg, fd) -> equilibrate
			   (if x = y then a
			   else if x < y then
			     Node (y, h+1, insert x fg, fd)
			   else
			     Node (y, h+1, fg, insert x fd))


let rec list2tree = function
  | [] -> Nil
  | h :: t -> insert h (list2tree t)

let tree2list a =
  let rec tree2list_aux accu = function
    | Nil ->  accu
    | Node (x, _, fg, fd) -> x :: (tree2list_aux (tree2list_aux accu fg) fd)
  in
  tree2list_aux [] a

(* Examples
let _ = list2tree [3;4;1;7;5;2]
let _ = list2tree [1;2;3;4;5;6;7;8;9]
let _ = tree2list (list2tree [1;2;3;4;5;6;7;8;9;10])
*)

(* max_tree et min_tree permettent de chercher le noeud maximal ou minimal *)
(* Le problËme c'est qu'on est en non-typÈ : donc que renvoyer pour 
 * 'min_tree Nil' ? je travaille donc en type option : on renvoie 'None' si
 * l'arbre AVL n'admet pas de minimum/maximum.
 * Je dÈfinis max_o et min_o qui calculent le maximum/minimum selon ces
 * conventions. *)
		
let max_o a b = match (a,b) with
  | (None, None) -> None
  | (None, _) -> b
  | (_, None) -> a
  | (Some(x), Some(y)) -> if x > y then a else b

let min_o a b = match (a,b) with
  | (None, None) -> None
  | (None, _) -> b
  | (_, None) -> a
  | (Some(x), Some(y)) -> if x < y then a else b

let rec max_tree = function
  | Nil -> None
  | Node (x, _, fg, fd) -> max_o (Some(x)) (max_o (max_tree fg) (max_tree fd))

let rec min_tree = function
  | Nil -> None
  | Node (x, _, fg, fd) -> min_o (Some(x)) (min_o (min_tree fg) (min_tree fd))


(* On en dÈduit la fonction pour supprimer un ÈlÈment d'un arbre AVL
 * Cette fonction renvoie un arbre dont l'ÈlÈment a ÈtÈ supprimÈ
 * qui possËde toujours la propriÈtÈ AVL *)
				 
let rec delete x = function
  | Nil -> Nil
  | Node (y, h, fg, fd) when y <> x -> let new_fg = if x < y then delete x fg else fg
				     and new_fd = if x > y then delete x fd else fd in
				     Node (y, 1 + max (height new_fg) (height new_fd), new_fg, new_fd)

  | Node (x, h, fg, fd) -> if fg = Nil && fd = Nil then Nil
			 else if height fg > height fd then
			     begin match max_tree fg with
				   | None -> failwith "None in 'delete'"
				   | Some(m) ->
				      let new_fg = delete m fg in
				      Node (m, 1 + max (height new_fg) (height fd), new_fg, fd)
			     end
			 else
			   begin match min_tree fd with
				 | None -> failwith "None in 'delete'"
				 | Some(m) ->
				    let new_fd = delete m fd in
				    Node (m, 1 + max (height new_fd) (height fg), fg, new_fd)
			   end
			     

(* Examples
let a = list2tree [0;1;2;3;4;5]
let _ = tree2list (delete 4 a)
let _ = delete 4 a
*)

(* Autres fonctions *)
let rec search x = function
  | Nil -> false
  | Node (y, _, fg, fd) -> if x = y then true
			   else if x < y then search x fg
			   else search x fd
