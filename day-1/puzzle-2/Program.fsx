
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
    let allNumbers = 
        numbersMap
        |> Map.fold (fun (acc: FoundNumberResult list) spelledNumber numberStr  -> 
            let indexesForSpelled = findAllSubstringIndexes str spelledNumber
            let indexesForDigit = findAllSubstringIndexes str numberStr
            let mapToFoundNumbers (indexes: int list) = 
                if indexes.Length > 0 then
                    indexes
                    |> List.map (fun index -> { number = numberStr; index = index })
                else
                    []
            let foundSpelled = mapToFoundNumbers indexesForSpelled
            let foundDigits = mapToFoundNumbers indexesForDigit
            acc 
            |> List.append foundSpelled 
            |> List.append foundDigits

        ) []
        |> List.sortBy (fun { index = i } -> i)
    let firstNumber = (allNumbers |> List.head).number
    let lastNumber = (allNumbers |> List.last).number
    $"{firstNumber}{lastNumber}"


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


