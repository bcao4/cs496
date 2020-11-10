
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let t1 = Node(30,Node(20,Node(10,Empty,Empty),Empty),Empty)
let t2 = Node(4,
              Node(3,
                   Node(2,
                        Node(1,Empty,Empty),
                        Empty),
                   Empty),
              Node(5,
                   Empty,
                   Node(6,
                        Empty,
                        Node(7,Empty,Empty))))

let t3 = Node(12,
              Node(7,Empty,Empty),
              Node(24,
                   Node(18,Empty,Empty),
                   Empty))

(** A list with the i-the level of t. 1 is the first level. 
   If the level is greater than the height of the tree, then 
   it should return the empty list 
*)
let rec level t i =
  match i,t with
  | _,Empty -> []
  | 1,Node(d,lt,rt) -> [d]
  | _,Node(d,lt,rt) -> level lt (i-1) @ level rt (i-1)
    
(** A list of lists with all the levels of the tree. 
    Return the empty list if the tree is Empty *)
let rec height t =
  match t with
  | Empty -> 0
  | Node(d,lt,rt) -> 1+max (height lt) (height rt)
                       
let rec levels t =
  let rec levels' t i =
    match i with
    | 0 -> []
    | n -> level t n :: levels' t (i-1)
  in levels' t (height t)

(** Perfect binary tree of a given height whose nodes contain d as
    data. The height is h is an integer greater or equal to zero.
*)
let rec pbt h d =
  match h with
  | 0 -> Empty
  | n -> Node(d, pbt (h-1) d, pbt (h-1) d)
           
(** A list with all the paths from the root to a leaf 
    eg: 
    # paths_to_leaves t2;;
    - : int list list = [[0; 0; 0]; [1; 1; 1]] 
*)      
let rec paths_to_leaves t =
  match t with
  | Empty -> []
  | Node(d,Empty,Empty) -> [[]]
  | Node(d,lt,rt) ->   List.map (fun i -> 0::i) (paths_to_leaves lt)
                       @
                       List.map (fun i -> 1::i) (paths_to_leaves rt)
                         
(** A list with all the paths from the root to any node. 
    eg: 
    # paths t2;;
    - : int list list = [[0; 0; 0]; [0; 0]; [0]; [1; 1; 1]; [1; 1];
    [1]; []]
    
    If the tree is empty, then paths returns the empty list []
*)  
let rec paths t =
  match t with
  | Empty -> []
  | Node(d,Empty,Empty) -> [[]]
  | Node(d,lt,rt) ->   List.map (fun i -> 0::i) (paths lt)
                       @
                       List.map (fun i -> 1::i) (paths rt)
                       @
                       [[]]
