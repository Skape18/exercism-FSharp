module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { Direction: Direction; Position: Position }

let directions = [|North; East; South; West;|]
let directionsCount = directions.Length

let create direction position = { Direction = direction; Position = position }

let getDirectionIndex direction = Array.findIndex ((=) direction) directions

let getNewDirection direction offset =
    let currentDirectionIndex = getDirectionIndex direction
    let nextDirectionIndex = (directionsCount + currentDirectionIndex + offset) % directionsCount
    directions.[nextDirectionIndex]

let turn robot offset = create (getNewDirection robot.Direction offset) robot.Position 
let turnLeft robot = turn robot -1
let turnRight robot = turn robot 1

let newPosition robot =
     match robot.Direction with
        | North -> (fst robot.Position, snd robot.Position + 1)
        | East -> (fst robot.Position + 1, snd robot.Position)
        | South -> (fst robot.Position, snd robot.Position - 1)
        | West ->(fst robot.Position - 1, snd robot.Position)

let advance robot = create robot.Direction (newPosition robot)

let performInstruction robot instruction =
    match instruction with
    | 'R' -> turnRight robot
    | 'L' -> turnLeft robot
    | 'A' -> advance robot

let rec move instructions robot = 
    match instructions with
    | "" -> robot
    | _  -> move instructions.[1..] (performInstruction robot instructions.[0])