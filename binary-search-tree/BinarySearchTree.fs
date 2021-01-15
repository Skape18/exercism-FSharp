module BinarySearchTree

type TreeData =
    Empty
    | Node of int * TreeData * TreeData

let left node = match node with
                | Empty
                | Node (_, Empty, _) -> None
                | Node (_, n, _) -> Some n

let right node = match node with
                 | Empty
                 | Node (_, _, Empty) -> None
                 | Node (_, _, n) -> Some n

let data node = match node with
                | Empty -> 0
                | Node (v, _, _) -> v

let create items =
    let rec addItem node item =
        match node with
        | Empty -> Node (item, Empty, Empty)  
        | Node (v, l, r) -> if (v >= item)
                            then Node (v, addItem l item, r)
                            else Node (v, l, addItem r item)

    List.fold (addItem) Empty items

let rec sortedData node = 
    match node with
    | Empty -> []
    | Node (v, l, r) -> (sortedData l) @ [v] @ (sortedData r) 