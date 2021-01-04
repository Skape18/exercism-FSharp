module KindergartenGarden

type Plant = Violets | Grass | Radishes | Clover

let matchPlant char =
    match char with
    | 'V' -> Violets
    | 'R' -> Radishes
    | 'C' -> Clover
    | 'G' -> Grass

let students =
    [|"Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry"|] 

let studentOffset (student: string) =
    2 * (Array.findIndex ((=) student) students)

let plants (diagram: string) student =
    let offset = studentOffset student
    diagram.Split [|'\n'|]
        |> Seq.map (fun line -> line.[offset..(offset+1)])
        |> Seq.collect id
        |> Seq.map matchPlant
        |> List.ofSeq


