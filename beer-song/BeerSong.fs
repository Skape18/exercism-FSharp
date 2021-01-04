module BeerSong

let bottleWord bottleCount =
    match bottleCount with
    | c when c > 1 -> "bottles"
    | c when c = 1 -> "bottle"

let firstLine bottles = 
    match bottles with
    | b when b >= 1 -> $"{b} {bottleWord b} of beer on the wall, {b} {bottleWord b} of beer."
    | b when b = 0 -> "No more bottles of beer on the wall, no more bottles of beer."

let secondLine bottles = 
    match bottles with
    | b when b >= 2 -> $"Take one down and pass it around, {b - 1} {bottleWord (b-1)} of beer on the wall."
    | b when b = 1 -> "Take it down and pass it around, no more bottles of beer on the wall."
    | b when b = 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."

let lines bottles = [firstLine bottles; secondLine bottles;]

let emptyLine = [""]

let rec recite (startBottles: int) (takeDown: int) = 
    if takeDown = 0 then []
    else if takeDown = 1 then lines startBottles @ recite (startBottles-1) (takeDown-1) 
    else lines startBottles @ emptyLine @ recite (startBottles-1 % 100) (takeDown-1)