module DotDsl

type NodeKey = string
type Attribute = string * string

type Node = NodeKey * Attribute list 
type Edge = NodeKey * NodeKey * Attribute list
type Element = Node of Node | Edge of Edge | Attribute of Attribute

let graph children = children |> List.sort

let attr key value = Attribute (key, value)

let node key attrs = Node (key, attrs)

let edge left right attrs = Edge (left, right, attrs) 

let attrs graph = graph |> List.filter (function Attribute _ -> true | _ -> false)

let nodes graph = graph |> List.filter (function Node _ -> true | _ -> false)

let edges graph =  graph |> List.filter (function Edge _ -> true | _ -> false)