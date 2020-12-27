module Accumulate

let rec reverse list =
    let rec revInt acc tail =
        match tail with
        | [] -> acc
        | head::tail' -> revInt (head::acc) tail' 
    revInt [] list


let rec accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec loop acc func tail =
        match tail with 
        | [] -> reverse acc
        | head :: tail' -> loop (func head::acc) func tail'
    loop [] func input

 
