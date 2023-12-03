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

let MAX_ROLL = {
    red = 12
    green = 13
    blue = 14
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

let gameIsPossible (game: Game) = 
    let rollIsPossible roll =
        roll.red <= MAX_ROLL.red && roll.blue <= MAX_ROLL.blue && roll.green <= MAX_ROLL.green
    game.rolls
    |> List.forall rollIsPossible

let answer =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
    |> Path.GetFullPath
    |> File.ReadAllLines
    |> Seq.map convertLineToGame
    |> Seq.filter gameIsPossible
    |> Seq.map (fun game -> game.id)
    |> Seq.sum

printfn "Answer: %A" answer
