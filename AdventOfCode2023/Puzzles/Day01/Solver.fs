module Day01

open System
open System.Collections
open AdventOfCode2023.Puzzles.Common

let input = getinputlines "01"

type NumOrNot =
    | Num of x:int
    | Not of y: string

let toNum (x:char) : NumOrNot =
    try
        Int32.Parse (x.ToString()) |> NumOrNot.Num
    with
    | ex -> x.ToString() |> NumOrNot.Not

let inputTransform (inputToTransform:Generic.IEnumerable<string>) =
      inputToTransform |> List.ofSeq |> Seq.map (Seq.map toNum)

let onlyNums listOfListOfNumOrNot =
    listOfListOfNumOrNot
    |> Seq.filter (fun c -> match c with
                         | Num a -> true
                         | Not _ -> false) // nao é númer
    |> Seq.map (fun i -> match i with
                         | Num a -> a
                         | Not _ -> 0)

let sumEachFirstAndLast (listOfListOfNumOrNot:seq<seq<int>>) =
    listOfListOfNumOrNot
    |> Seq.map (fun list -> match list with
                            | a when Seq.length a = 1 -> (Seq.item 0 a) * 11
                            | b when Seq.length b < 1 -> 0
                            | c when Seq.length c > 1 -> ((Seq.item 0 c) * 10) + Seq.last c)
let sumAllSums (listInts:seq<int>) =
    listInts
    |> Seq.sum
    
let puzzleAnswer =
    input
    |> inputTransform
    |> Seq.map(onlyNums)
    |> sumEachFirstAndLast
    |> sumAllSums
    
    