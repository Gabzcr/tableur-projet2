open Cell
open Sheet
open Command

(*** d�but de la partie "incantatoire" ***)
(* stdin d�signe l'entr�e standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)
let lexbuf = Lexing.from_channel stdin

(* on encha�ne les tuyaux: lexbuf est pass� � Lexer.token,
   et le r�sultat est donn� � Parser.main *)

let parse () = Parser.debut Lexer.token lexbuf
(*** fin de la partie "incantatoire" ***)

let spreadsheet () =
      let result = parse () in
      begin
        run_script result;
        flush stdout;
      end
;;


let _ = spreadsheet()

(* let _ = let scr = [ Upd (("C",2), Cst 3.7); ShowAll ] in
 *         begin
 *           print_string "HAHAHA!!!\n";
 *           run_script scr
 *         end *)
