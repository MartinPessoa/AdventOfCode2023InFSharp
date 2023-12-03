module AdventOfCode2023.Main

open AdventOfCode2023.Puzzles.Day01.SolverPart2

[<EntryPoint>]
let main argv =
    intermediary
    printfn $"The answer is: {puzzleAnswer}"
    0 // return an integer exit code