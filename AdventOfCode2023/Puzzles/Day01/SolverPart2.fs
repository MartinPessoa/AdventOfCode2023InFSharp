module AdventOfCode2023.Puzzles.Day01.SolverPart2
open AdventOfCode2023.Puzzles.Common

let input = getinputlines "01"

let convertLiteralsToInts (string:string) =
    string
        .Replace("oneight", "18")
        .Replace("nineight", "98")
        .Replace("threeight", "38")
        .Replace("fiveight", "58")
        .Replace("sevenine", "79")
        .Replace("eightwo", "82")
        .Replace("eighthree", "83")
        .Replace("twone", "21")
        .Replace("one", "1")
        .Replace("two", "2")
        .Replace("three", "3")
        .Replace("four", "4")
        .Replace("five", "5")
        .Replace("six", "6")
        .Replace("seven", "7")
        .Replace("eight", "8")
        .Replace("nine", "9")
    
let sumFirstAndLast (listOfNumOrNot:seq<int>) =
   match listOfNumOrNot with
                    | a when Seq.length a = 1 -> (Seq.item 0 a) * 11
                    | b when Seq.length b < 1 -> 0
                    | c when Seq.length c > 1 -> ((Seq.item 0 c) * 10) + Seq.last c
                    | _ -> 0
                    
let sumEachFirstAndLast (listOfListOfNumOrNot:seq<seq<int>>) =
    listOfListOfNumOrNot
    |> Seq.map sumFirstAndLast
    
let isNumeral (c:char) =
    match c with
        | n when n = '1' -> true
        | n when n = '2' -> true
        | n when n = '3' -> true
        | n when n = '4' -> true
        | n when n = '5' -> true
        | n when n = '6' -> true
        | n when n = '7' -> true
        | n when n = '8' -> true
        | n when n = '9' -> true
        | _ -> false
        
let toInt (c:char) =
    match c with
        | n when n = '1' -> 1
        | n when n = '2' -> 2
        | n when n = '3' -> 3
        | n when n = '4' -> 4
        | n when n = '5' -> 5
        | n when n = '6' -> 6
        | n when n = '7' -> 7
        | n when n = '8' -> 8
        | n when n = '9' -> 9
        | _ -> 0
        
let puzzleAnswer =
    input
    |> Seq.map(convertLiteralsToInts)
    |> Seq.map(Seq.filter(isNumeral))
    |> Seq.map(Seq.map(toInt))
    |> sumEachFirstAndLast
    |> Seq.sum
    
let intermediary =
    input
    |> Seq.iter (fun i ->
        printf $"{i} - "
        printf $"{convertLiteralsToInts i} - "
        printf $"{i |> convertLiteralsToInts |> Seq.filter(isNumeral) |> Seq.toArray |> System.String   } - "
        printfn $"{i |> convertLiteralsToInts |> Seq.filter(isNumeral) |> Seq.map(toInt) |> sumFirstAndLast  } - "
        )