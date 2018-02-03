open Cell
open Sheet
open Command

(*** début de la partie "incantatoire" ***)
(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)
let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.debut Lexer.token lexbuf
(*** fin de la partie "incantatoire" ***)

let spreadsheet () =
      let speclist = [("-naive", Arg.Set naive, "Switch to naive mode")]
      in let usage_msg = "\nThis is an ocaml program to work on a grid of values defined by formulas.\n";
      in Arg.parse speclist print_endline usage_msg;
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
