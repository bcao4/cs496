type dir = North | East | South | West
type snake = dir list 
type event = Apple | Move of dir
type run = event list

let rec dropLast = function
    | [] -> failwith "Not Possible"
    | [_] -> []
    | d::t -> d:: dropLast t

let move : snake -> snake = fun s ->
    List.hd s :: dropLast s

let eat_apple s = List.hd s :: s

let conflicting : dir -> dir -> bool = fun d1 d2 ->
    match d1,d2 with
    | North,South | South,North | West,East | East,West -> true
    | _ -> false

let change_dir s newdir =
    if conflicting newdir (List.hd s)
    then move s
    else newdir :: dropLast s

let prev(x,y) = function
    | North -> (x,y-1)
    | East -> (x-1,y)
    | South -> (x, y+1)
    | West -> (x+1, y)

let rec coverage ((x,y):int*int) (s:snake) : (int*int) list =
    match s with
    | [] -> []
    | d::t -> (x,y) :: coverage (prev (x,y) d) t

let rec has_duplicates : 'a list -> bool = function
    | [] -> false
    | h::t -> List.mem h t || has_duplicates t

let bites_tail s =
    has_duplicates (coverage (0,0) s)