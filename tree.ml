(************* Un fichier pour manipuler des arbres **************)

(* AVL trees *)
type 'a searchTree = Nill | Node of 'a * int * 'a searchTree * 'a searchTree

let height = function
  | Nill -> 0
  | Node (_, h, _, _) -> h

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
  | Nill -> Nill
  | Node (_, _, fg, fd) -> let hg = height fg
			  and hd = height fd in
			  if (hg + 1) > hd then rotate_right a
			  else if (hd + 1) > hg then rotate_left a
			  else a

let rec insert x a = match a with
  | Nill -> Node (x, 1, Nill, Nill)
  | Node (y, h, fg, fd) -> equilibrate
			   (if x = y then a
			   else if x < y then
			     Node (y, h+1, insert x fg, fd)
			   else
			     Node (y, h+1, fg, insert x fd))


let rec list2tree = function
  | [] -> Nill
  | h :: t -> insert h (list2tree t)

let tree2list a =
  let rec tree2list_aux accu = function
    | Nill ->  accu
    | Node (x, _, fg, fd) -> x :: (tree2list_aux (tree2list_aux accu fg) fd)
  in
  tree2list_aux [] a

(* Examples
let _ = list2tree [3;4;1;7;5;2]
let _ = list2tree [1;2;3;4;5;6;7;8;9]
let _ = tree2list (list2tree [1;2;3;4;5;6;7;8;9;10])
*)

let rec max_tree = function
  | Nill -> min_int
  | Node (x, _, fg, fd) -> max x (max (max_tree fg) (max_tree fd))

let rec min_tree = function
  | Nill -> max_int
  | Node (x, _, fg, fd) -> min x (min (min_tree fg) (min_tree fd))

let rec delete x = function
  | Nill -> Nill
  | Node (y, h, fg, fd) when y <> x -> let new_fg = if x < y then delete x fg else fg
				     and new_fd = if x > y then delete x fd else fd in
				     Node (y, 1 + max (height new_fg) (height new_fd), new_fg, new_fd)

  | Node (x, h, fg, fd) -> if fg = Nill && fd = Nill then Nill
			 else if height fg > height fd then
			   begin let m = max_tree fg in
				 let new_fg = delete m fg in
				 Node (m, 1 + max (height new_fg) (height fd), new_fg, fd)
			   end
			 else
			   begin let m = min_tree fd in
				 let new_fd = delete m fd in
				 Node (m, 1 + max (height new_fd) (height fg), fg, new_fd)
			   end

(* Examples
let a = list2tree [0;1;2;3;4;5]
let _ = tree2list (delete 4 a)
let _ = delete 4 a
*)
