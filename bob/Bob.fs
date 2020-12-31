module Bob

let lastChar s = s |> Seq.last

let isQuestion s = lastChar s = '?'

let containsLetter s = 
    s|> Seq.exists System.Char.IsLetter

let isUpper (s: string) = s = s.ToUpper() 

let response (input: string): string = 
    match input.Trim() with
    | i when System.String.IsNullOrWhiteSpace i -> "Fine. Be that way!"
    | i when containsLetter i && isUpper i && isQuestion i -> "Calm down, I know what I'm doing!"
    | i when containsLetter i && isUpper i -> "Whoa, chill out!"
    | i when isQuestion i -> "Sure."
    | _ -> "Whatever."