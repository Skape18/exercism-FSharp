module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let getStudentsByGrade (number: int) (school: School): string list = 
    match school.TryFind number with
    | Some(x) -> x
    | None -> []

let buildStudentsList student grade school = (student::(getStudentsByGrade grade school)) |> List.sortBy id

let add (student: string) (grade: int) (school: School): School = 
    school.Add(grade, buildStudentsList student grade school)

let roster (school: School): string list = 
    if school.IsEmpty then []
    else school
            |> Map.toSeq
            |> Seq.sortBy (fun (k, _) -> k)
            |> Seq.fold (fun acc v -> acc @ snd v) []  

let grade (number: int) (school: School): string list = getStudentsByGrade number school
