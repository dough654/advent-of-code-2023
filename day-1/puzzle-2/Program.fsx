open System.IO
open System.Text.RegularExpressions

let numbersMap = Map.ofList [
    ("zero", "0")
    ("one", "1")
    ("two", "2")
    ("three", "3")
    ("four", "4")
    ("five", "5")
    ("six", "6")
    ("seven", "7")
    ("eight", "8")
    ("nine", "9")
]


type FoundNumberResult = {
    number: string
    index: int
}

let findAllSubstringIndexes (input: string) (pattern: string) =
    let matches = Regex.Matches(input, pattern)
    [for m in matches do
        yield m.Index]

let getCombinedFirstAndLastNumbers (str: string) =
    let allFoundNumbers = 
        numbersMap
        |> Map.fold (fun (acc: FoundNumberResult list) spelledNumber numberStr  -> 
            let indexesForSpelled = findAllSubstringIndexes str spelledNumber
            let indexesForDigit = findAllSubstringIndexes str numberStr
            let mapToFoundNumbers index = { number = numberStr; index = index }
            let foundSpelled = indexesForSpelled |> List.map mapToFoundNumbers
            let foundDigits = indexesForDigit |> List.map mapToFoundNumbers
            acc @ foundSpelled @ foundDigits
        ) []
        |> List.sortBy (fun { index = i } -> i)
    let firstNumber = (allFoundNumbers |> List.head).number
    let lastNumber = (allFoundNumbers |> List.last).number
    firstNumber + lastNumber

let answer =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
    |> Path.GetFullPath
    |> File.ReadAllLines
    |> Seq.map (fun line -> 
        line
        |> getCombinedFirstAndLastNumbers
        |> int
    )
    |> Seq.sum

printfn "%A" answer


