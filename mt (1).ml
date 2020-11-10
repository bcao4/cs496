
(* 

    Quiz 1 - Section A 

*)

type 'a tree = Empty | Node of 'a*'a tree*'a tree

let rec take n xs =
  match n,xs with
  | 0,_ -> []
  | n,[] -> []
  | n,h::t -> h :: take (n-1) t

let rec drop n xs =
  match n,xs with
  | 0,ys -> ys
  | n,[] -> []
  | n,h::t -> drop (n-1) t


let split xs =
  let s = List.length xs
  in (take (s/2) xs, drop (s/2) xs) 

let rec mt f xs =
  match xs with
  | [x] -> Node(f x,Empty,Empty)
  | _ ->
    let (left,right) = split xs
    in let Node(i,lti,rti) = mt f left
    in let Node(j,ltj,rtj) = mt f right
    in Node(f (i+j),Node(i,lti,rti),Node(j,ltj,rtj))
  
let rec mt' f xs =
  match xs with
  | [x] -> Node(f x,Empty,Empty)
  | _ ->
    let (left,right) = split xs
    in
    (match mt f left, mt f right with
     | (Node(i,lti,rti) as l),(Node(j,ltj,rtj) as r)
       -> Node(f (i+j),l,r)
     | _ -> failwith "cannnot arise")
  
