 module QueenAttack

let inBoardRange (position: int) = position < 8 && position >= 0;

let create (position: int * int) =  inBoardRange (fst position) && inBoardRange (snd position);

let canAttack (queen1: int * int) (queen2: int * int) = 
    fst queen1 = fst queen2 
    || snd queen1 = snd queen2 
    || abs(fst queen1 - fst queen2) = abs(snd queen1 - snd queen2);