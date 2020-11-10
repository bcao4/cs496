
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]
        }

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
        }

let a3 = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q0",'a',"q2")];
         final = ["q2"]
        }

let a4 = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2"); ("q0",'a',"q2")];
         final = ["q2";"q5"]
        }

let a5 = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2";"q5"]
        }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]

(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let rec apply_transition_function : tf -> state -> symbol -> state option = fun tf state symbol ->  match tf with
  | [] -> None
  | (q0,sym,q1) :: tf when q0 = state && sym = symbol -> Some q1
  | (q0,sym,q1) :: tf -> apply_transition_function tf state symbol

let rec accept : fa -> input -> bool = fun fa input ->
  let rec accept_helper state input = 
    match input with
    | [] -> state
    | symbol::tail ->
      match state with
      | Some q -> accept_helper(apply_transition_function fa.tf q symbol) tail
      | None -> None
  in match accept_helper(Some fa.start) input with
  | None -> false
  | Some state -> List.mem state fa.final

let rec next : tf -> state -> symbol -> state list = fun tf state symbol ->
  match tf with
  | [] -> []
  | (q0, sym, q1) :: tail -> 
    if q0 = state && sym = symbol
    then q1 :: next tail state symbol
    else next tail state symbol

let rec check_tf = function
  | [] -> true 
  | (q0,sym,q1) :: tf -> (*Compare the next of current transition function with rest of list*)
    match next tf q0 sym with (*Takes q0 and sym of current tf and calls next on rest of list. If next returns empty, move on to rest of list, else it is an NFA*)
    | [] -> check_tf tf
    | _ -> false

let rec deterministic : fa -> bool = fun fa ->
  check_tf fa.tf

let rec element_of e l =
  match l with
  | [] -> false
  | hd::tl -> 
    if hd = e 
    then true
    else element_of e tl

let check_final_state : fa -> bool = fun fa ->
  let final_list = fa.final in
  let rec check1 final_list = 
    match final_list with
    | [] -> true
    | hd::tl -> 
      if(List.mem hd fa.states)
      then check1 tl
      else false
  in check1 final_list

let valid : fa -> bool = fun fa ->
  let start_state = fa.start in 
  let list_states = fa.states in 
  if( (element_of start_state list_states) && (check_final_state fa) && (deterministic fa) )
  then true
  else false

(*Lists all states reachable from start - with duplicates*)
let rec reachable_helper : tf -> state -> state list -> state list = fun tf state states ->
  match tf with
  | [] -> []
  | (q0, sym, q1)::tl ->
    if q0=state && q1<>state
    then
      if List.mem q1 states
      then reachable_helper tl state states
      else q1 :: reachable_helper tl state states
    else q1 :: reachable_helper tl state states

(*Appends start state to list of reachable states*)
let rec reachable_help: fa -> state list = fun fa ->
  let reach_list = reachable_helper fa.tf fa.start [] in
  List.append [fa.start] reach_list

(*Removes duplicates from reachable states*)
let rec reach_duplicates : state list -> state list = fun states ->
  match states with
  | [] -> []
  | hd::tl -> hd::(reach_duplicates (List.filter (fun q0 -> q0<>hd) tl))

let rec reachable : fa -> state list = fun fa ->
  let reach_list = reachable_help fa in
  reach_duplicates reach_list

let rec remove_dead_states : fa -> fa = fun fa ->
  let new_states = reachable fa in
  {
    states = new_states;
    tf = fa.tf;
    start = fa.start;
    final = fa.final;
  }