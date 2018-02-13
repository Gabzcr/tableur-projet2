open Debug
open Cell
open Sheet

(* commandes: ce que l'utilisateur peut saisir dans un fichier.
 - La modification d'une cellule avec une nouvelle formule,
 - l'affichage d'une cellule,
 - l'affichage de toute la feuille *)
type sheetname = string*int;;
type comm = Upd of cellname * form | Show of cellname | ShowAll | SwitchTo of sheetname


(************ affichage **************)
let show_comm c =
(* Fonction de déboggage pour afficher une commande *)
  match c with
  | Upd (c,f) ->
     begin
       ps (cell_name2string c);
       ps"=";
       show_form f
     end
  | Show c ->
     begin
       ps "Show(";
       ps (cell_name2string c);
       ps ")"
     end
  | ShowAll -> ps "ShowAll"
  | SwitchTo (s,i) ->
    begin
      ps "SwitchTo ";
      ps s;
      print_int i;
    end

(************ faire tourner les commandes **************)

(* exécuter une commande *)
let run_command c = match c with
  | Show cn ->
     begin
       let co = cellname_to_coord cn in
       let c = read_cell co in
       if c.value = None then
         sheet_iter eval_cell;
       eval_p_debug (fun () ->
           "Showing cell "
           ^ cell_name2string cn
         );
       ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
       print_newline()
     end
  | ShowAll ->
     begin
       let c = read_cell (0,0) in
       if c.value = None then
         sheet_iter eval_cell;
       eval_p_debug (fun () -> "Show All\n");
       show_sheet ()
     end
  | Upd(cn,f) ->
    begin
       let co = cellname_to_coord cn in
       eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");
       if !naive || (not !naive && uncycling_formula co f) then
        begin
          if not(!naive) then update_back_dependancies co f; (* attention à l'ordre ici pour ne pas perdre l'actuelle formule *)
          (* cas naif: tous les arbres de dépendances restent toujours à Nil et on les ignore *)
          update_cell_formula co f;
         if !naive then invalidate_sheet ()
         else update_up_dependancies co
        end
       else p_debug ("\tSkip" ^ "=" ^ (form2string f) ^ "\n")
    end
  | SwitchTo(s,i) -> feuille_courante := i;
;;

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
