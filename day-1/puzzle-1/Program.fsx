open System.IO

let firstAndLastOfList list =
    match list with
    // Default to 0 if empty list so will not affect the answer
    | [] -> ['0'; '0']
    | [x] -> [x; x]
    | first :: rest -> [first; List.last rest]

let answer =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
    |> File.ReadAllLines
    |> Seq.map (fun line -> 
        line
        |> List.ofSeq 
        |> List.filter System.Char.IsDigit
        |> firstAndLastOfList
        |> System.String.Concat
        |> int
    )
    |> Seq.sum
    |> int

printfn "%A" answer


let lines = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
    |> File.ReadAllLines

printfn "%A" lines.Length
