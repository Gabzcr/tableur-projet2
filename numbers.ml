

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

(* Fonctions préliminaires *)

let extract_value = function
  | Some(x) -> x
  | _ -> Int(0)
;;

let is_zero = function
  | (Int x) -> (x = 0)
  | (Float x) -> (x = 0.)
;;

(* Fonctions de calcul *)
(* Ces dernières servent principalement dans eval_form *)

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

let min_number x y = match x,y with
  | Int(i1), Int(i2) -> if i1 < i2 then x else y
  | Float(f1), Int(i2) -> if f1 < float_of_int i2 then x else y
  | Int(i1), Float(f2) -> if float_of_int i1 < f2 then x else y
  | Float(f1), Float(f2) -> if f1 < f2 then x else y

let minus_number x y = match x,y with
  | Some(Int(i1)), Some(Int(i2)) -> Some(Int(i1 - i2))
  | Some(Float(f1)), Some(Int(i2)) -> Some(Float(f1 -. float_of_int i2))
  | Some(Int(i1)), Some(Float(f2)) -> Some(Float(float_of_int i1 -. f2))
  | Some(Float(f1)), Some(Float(f2)) -> Some(Float(f1 -. f2))
  | None, Some(x) -> Some(x)
  | _,_ -> Some(Int 0)
;;

let generalised_div_number x y = match x,y with
  | Some(Int(i1)), Some(Int(i2)) -> Some(Int(i1 / i2))
  | Some(Float(f1)), Some(Int(i2)) -> Some(Float(f1 /. float_of_int i2))
  | Some(Int(i1)), Some(Float(f2)) -> Some(Float(float_of_int i1 /. f2))
  | Some(Float(f1)), Some(Float(f2)) -> Some(Float(f1 /. f2))
  | None, Some(x) -> Some(x)
  | _,_ -> Some(Int 1)

let mod_number x y = match x,y with
(* Pas de modulo sur les flottants *)
  | Some(Int(i1)), Some(Int(i2)) -> Some(Int(i1 mod i2))
  | None, Some(x) -> Some(x)
  | _,_ -> Some(Int 1)
