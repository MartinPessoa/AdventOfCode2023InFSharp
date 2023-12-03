module  AdventOfCode2023.Puzzles.Common
open System.Collections
    let readlines filepath = System.IO.File.ReadLines(filepath)

    let getinputlines day = readlines @$"D:\Repos\class-strugle\AdventOfCode\AdventOfCode2023\Puzzles\Day{day}\input.txt"
    
    let printalllines (range : Generic.IEnumerable<string>) =
      range |> List.ofSeq |> List.iter (printfn "%s")