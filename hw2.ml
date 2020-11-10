type dTree = Leaf of int | Node of char * dTree * dTree 

let tLeft = Node('w', 
                Node('x', 
                    Leaf(2), Leaf(5)),
                Leaf(8)) 

let tRight = Node('w',
                Node('x',
                    Leaf(2), Leaf(5)),
                Node('y',
                    Leaf(7), Leaf(5)))

let test_tree = Node('x',
                    Node('y',
                        Node('z',
                            Leaf(0), Leaf(1)),
                        Node('z',
                            Leaf(1), Leaf(0))),
                    Node('y',
                        Node('z',
                            Leaf(0), Leaf(1)),
                        Node('z',
                            Leaf(1), Leaf(0))))

let rec dTree_height : dTree -> int = fun dTree ->
  match dTree with
  | Leaf(a) -> 0
  | Node(d,lt,rt) -> 1 + max (dTree_height lt)(dTree_height rt)

let rec dTree_size : dTree -> int = fun dTree ->
    match dTree with
    | Leaf(a) -> 1
    | Node(d,lt,rt) -> 1 + (dTree_size lt) + (dTree_size rt)

let rec dTree_paths : dTree -> int list list = fun dTree ->
    match dTree with
    | Leaf(a) -> [[]]
    | Node(d,lt,rt) -> List.map (fun i -> 0::i) (dTree_paths lt)
                       @
                       List.map (fun i -> 1::i) (dTree_paths rt)

let rec dTree_is_perfect : dTree -> bool = fun dTree ->
    match dTree with 
    | Leaf(a) -> true
    | Node(d,lt,rt) -> 
        if dTree_height lt = dTree_height rt
        then true && dTree_is_perfect lt && dTree_is_perfect rt
        else false

let rec dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree = fun f g t -> 
    match t with
    | Leaf(a) -> Leaf(g a)
    | Node(d,lt,rt) -> Node(f d, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree : char list -> dTree = fun chars ->
    match chars with
    | [] -> failwith "Empty"
    | [a] -> Node(a, Leaf(0), Leaf(0))
    | hd::tl -> Node(hd, list_to_tree tl, list_to_tree tl)

let rec replace_leaf_at_helper : dTree -> int list -> int -> dTree = fun dTree num_list num ->
    match dTree with   
    | Leaf(a) -> Leaf(num)
    | Node(d,lt,rt) ->
        match num_list with
        | [] -> Node(d,lt,rt)
        | [a] ->
            if a = 0
            then Node(d, Leaf(num), rt)
            else Node(d, lt, Leaf(num))
        | hd::tl -> 
            if hd = 0
            then Node(d, replace_leaf_at_helper lt tl num, rt)
            else Node(d, lt, replace_leaf_at_helper rt tl num)

let rec replace_leaf_at : dTree -> (int list * int) list -> dTree = fun dTree graph -> 
    match graph with 
    | [] -> failwith "Empty"
    | [a,b] -> replace_leaf_at_helper dTree a b 
    | (hd, num)::tl -> replace_leaf_at (replace_leaf_at_helper dTree hd num) tl

let rec bf_to_dTree : char list * (int list * int) list -> dTree = fun graph ->
    let tree = list_to_tree (fst graph) in
        replace_leaf_at tree (snd graph)