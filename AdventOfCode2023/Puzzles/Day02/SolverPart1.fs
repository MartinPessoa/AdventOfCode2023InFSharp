module AdventOfCode2023.Puzzles.Day02.SolverPart1

open System
open AdventOfCode2023.Puzzles.Common
open Microsoft.FSharp.Collections
let input = getinputlines "02"

type Color =
    | Red
    | Green
    | Blue
    | Unknown

type RevealedCubes =
    {
        Red: int
        Green: int
        Blue: int
    }
    
type Game =
    {
        Id: int
        RevealedCubes: RevealedCubes array
    }

let calculateQuantityForColor(color:Color, revealed:(Color * string) array ) =
    let colors = Array.map (fun (c:Color,s) -> c) revealed
    
    match revealed with
        | z when Array.contains color colors = false -> 0
        | _ -> Array.find ( fun (c,r) -> c = color) revealed |> snd |> Int32.Parse
        
let makeRevealeds (revealed:(Color * string) array) =
    let redQty = calculateQuantityForColor (Red, revealed)
    let greenQty = calculateQuantityForColor (Green, revealed)
    let blueQty =  calculateQuantityForColor (Blue, revealed)
    { Red = redQty; Green = greenQty; Blue = blueQty }
    

let makeGame (id:string, revealed:(Color * string) array array) =
    let idInt = Int32.Parse id
    let list = Array.map makeRevealeds revealed
    {Id = idInt; RevealedCubes = list }

// receives:
// 3 blue
let qtyColor (string:String) =
    let split = string.Split(' ')
    let qty = split.[0]
    let color = match split.[1] with
                | r when r = "red" -> Red
                | g when g = "green" -> Green
                | b when b = "blue" -> Blue
                | _ -> Unknown
    (color, qty)

// receives:
// 3 blue, 4 red
let splitColors map (string:String) =
    Array.map map (string.Split(','))

// receives:
// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
let splitGame map (string:String) =
    let firstPass = string.Split(':')
    let gameId = firstPass.[0].Replace("Game ", "") // only the number remains
    let mapped = Array.map map (firstPass.[1].Split(';')) // will pass array of: ["3 blue, 4 red", "3 blue, 4 red"] etc
    (gameId, mapped)

let printGames (games:Game seq) = 
    Seq.iter (fun g -> printfn $"Game id: {g.Id}") games
        
let puzzleAnswer = input |> Array.ofSeq |> Seq.map (splitGame (splitColors(qtyColor))) |> Seq.map makeGame

