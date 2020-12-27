module Raindrops

let getRaindropString (number: int) =
    [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    |> List.map (fun (num, str) -> if number % num = 0 then str else "")
    |> String.concat ""

let convert (number: int): string = 
    match getRaindropString number with
    | "" -> number.ToString()
    | v -> v
