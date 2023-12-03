open System.IO

type Roll = {
    red: int
    blue: int
    green: int
}

type Game = {
    id: int
    rolls: Roll list
}

let getGameIdFromLine (line: string) =
    int (line.Split(":").[0].Split(" ").[1])

let getRollsFromLine (line: string) = 
    line.Split(":").[1].Split(";")
    |> Array.map (fun rollSection -> 
        let colorPairs = 
            rollSection.Split(",")
            |> Array.map (fun pair -> pair.Trim().Split(" ") |> List.ofArray)
            |> List.ofArray
        colorPairs
        |> List.fold (fun (acc: Roll) colorPair -> 
            match colorPair with
            | [numStr; "blue"] -> { acc with blue = acc.blue + (int numStr)}
            | [numStr; "red"] -> { acc with red = acc.red + (int numStr)}
            | [numStr; "green"] -> { acc with green = acc.green + (int numStr)}
            | _ -> acc
        ) { blue = 0; red = 0; green = 0}
    )
    |> List.ofArray

let convertLineToGame (line: string) =
    { 
        id = getGameIdFromLine line
        rolls = getRollsFromLine line
    }

let minimumRollForGame (game: Game) = 
    let minValue = System.Int32.MinValue
    game.rolls
    |> List.fold (fun (acc: Roll) roll ->
        let newRed = if roll.red > acc.red then roll.red else acc.red
        let newBlue = if roll.blue > acc.blue then roll.blue else acc.blue
        let newGreen = if roll.green > acc.green then roll.green else acc.green
        { red = newRed; blue = newBlue; green = newGreen}
    ) { red = minValue; blue = minValue; green = minValue }

let powerForRoll (roll: Roll) =
    roll.red * roll.blue * roll.green

let answer =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
    |> Path.GetFullPath
    |> File.ReadAllLines
    |> Seq.map convertLineToGame
    |> Seq.map minimumRollForGame
    |> Seq.map powerForRoll
    |> Seq.sum

printfn "Answer: %A" answer
