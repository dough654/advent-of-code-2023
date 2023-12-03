open System.IO
open System.Text.RegularExpressions

type Number = {
    value: int
    rowIndex: int
    colIndexRange: (int * int)
}

type Symbol = {
    value: string
    rowIndex: int
    colIndex: int
}

let getNumbersForLine (line: string) rowIndex = 
    let matches = Regex.Matches(line, "\\d+")
    [for m in matches do
        yield { value = (int m.Value); rowIndex = rowIndex; colIndexRange = (m.Index, m.Index + m.Value.Length - 1) }]

let getSymbolsLine (line: string) rowIndex = 
    let matches = Regex.Matches(line, "\\*")
    [for m in matches do
        yield { value = m.Value; rowIndex = rowIndex; colIndex = (int m.Index) }]

let symbolIsInRange (symbol: Symbol) (num: Number) =
    let (start, finish) = num.colIndexRange
    let result = symbol.colIndex >= (start - 1) && symbol.colIndex <= (finish + 1)
    result

let getGearRatioForSymbol (numbers: Map<int, Number list>) (symbol: Symbol) =
    let numbersAbove = numbers.TryFind (symbol.rowIndex - 1)
    let numbersSameLine = numbers.TryFind symbol.rowIndex
    let numbersBelow = numbers.TryFind (symbol.rowIndex + 1)
    let numbersTouched =
        [numbersAbove; numbersSameLine; numbersBelow]
        |> List.fold (fun (acc: Number list) maybeNumbers -> 
            match maybeNumbers with
            | None -> acc // No symbols on that line
            | Some numbers -> acc @ List.filter (symbolIsInRange symbol) numbers
        ) []
    if numbersTouched.Length = 2 then 
        numbersTouched 
        |> List.fold (fun acc num -> acc * num.value) 1
    else
        0

let answer =
    let lines = 
        Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
        |> Path.GetFullPath
        |> File.ReadAllLines
    let numbersMap = 
        lines
        |> Seq.mapi (fun i line -> getNumbersForLine line i)
        |> Seq.collect id
        |> Seq.fold (fun (acc: Map<int, Number list>) num ->
            if acc.ContainsKey num.rowIndex then 
                acc.Add(num.rowIndex, (num :: acc.[num.rowIndex]))
            else 
                acc.Add(num.rowIndex, [num])
        ) Map.empty
    let symbols = 
        lines
        |> Seq.mapi (fun i line -> getSymbolsLine line i)
        |> Seq.collect id

    let total = 
        symbols
        |> Seq.map (getGearRatioForSymbol numbersMap) 
        |> Seq.sum
    total

printfn "Answer: %A" answer
