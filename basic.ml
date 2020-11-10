
(* 24 Jan 2020 *)

(* examples of recursion on numbers *)

let rec fact (n:int):int =
  match n with
  | 0 -> 1
  | m -> m * fact (m-1)

let rec sum (n:int):int =
  match n with
  | 0 -> 0
  | m when m>0 -> m + sum (m-1)
  | _ -> failwith "Not defined on negative numbers"

(* examples of recursion on lists *)
let rec sizel (l:'a list):int =
  match l with
  | [] -> 0
  | h::t -> 1 + sizel t

let rec suml (l:int list):int =
  match l with
  | [] -> 0
  | h::t -> h + suml t

let rec repeat e n =
  match n with
  | 0 -> []
  | m -> e::repeat e (m-1)

let rec repeat' e = function
  | 0 -> []
  | m -> e::repeat' e (m-1)

let rec s (l:'a list):'a list =
  match l with
  | [] -> []
  | h::t -> h::h::s t

let rec s' (n:int) (l:'a list):'a list =
  match l with
  | [] -> []
  | h::t -> repeat h n @ s' n t

let rec rad = function
  | [] -> []
  | [x] -> [x]
  | x::y::t when x=y -> rad (y::t)
  | x::y::t -> x:: rad (y::t)

