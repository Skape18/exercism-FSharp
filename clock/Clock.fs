module Clock

type Clock = { Minutes: int }

let day = 24 * 60;

let minutesToClock minutes = 
    { 
        Minutes = 
            match minutes with
            | x when x >= 0 -> minutes % day
            | x when x < 0  -> (day + minutes % day) % day 
    }

let create hours minutes = 
    minutesToClock (hours * 60 + minutes)

let add minutes clock = minutesToClock (clock.Minutes + minutes)

let subtract minutes clock = minutesToClock (clock.Minutes - minutes)

let display clock =
    let minutes = clock.Minutes % 60
    let hours = clock.Minutes / 60
    sprintf "%02d:%02d" hours minutes
