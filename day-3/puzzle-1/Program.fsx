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
    let matches = Regex.Matches(line, "[^0-9.]")
    [for m in matches do
        yield { value = m.Value; rowIndex = rowIndex; colIndex = (int m.Index) }]

let symbolIsInRange (num: Number) (symbol: Symbol) =
    let (start, finish) = num.colIndexRange
    // printfn "Checking if %A on line %A is touching symbol %A on line %A" num.value num.rowIndex symbol.value symbol.rowIndex
    // printfn "Symbol is at index %A, were checking range %A->%A" symbol.colIndex (start - 1) (finish + 1)
    let result = symbol.colIndex >= (start - 1) && symbol.colIndex <= (finish + 1)
    // printfn "Result %A" result
    result

let numberIsValid (symbols: Map<int, Symbol list>) (num: Number) =
    let symbolsAbove = symbols.TryFind (num.rowIndex - 1)
    let symbolsSameLine = symbols.TryFind num.rowIndex
    let symbolsBelow = symbols.TryFind (num.rowIndex + 1)
    let touchesSymbol = 
        [symbolsAbove; symbolsSameLine; symbolsBelow]
        |> List.exists (fun maybeSymbols -> 
            match maybeSymbols with
            | None -> false // No symbols on that line
            | Some symbols -> List.exists (symbolIsInRange num) symbols
        ) 
    // printfn "Number %A Touches symbol %A" num.value touchesSymbol
    touchesSymbol

let answer =
    let lines = 
        Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
        |> Path.GetFullPath
        |> File.ReadAllLines
    let numbers = 
        lines
        |> Seq.mapi (fun i line -> getNumbersForLine line i)
        |> Seq.collect id
    let symbolsMap = 
        lines
        |> Seq.mapi (fun i line -> getSymbolsLine line i)
        |> Seq.collect id
        |> Seq.fold (fun (acc: Map<int, Symbol list>) num ->
            if acc.ContainsKey num.rowIndex then 
                acc.Add(num.rowIndex, (num :: acc.[num.rowIndex]))
            else 
                acc.Add(num.rowIndex, [num])
        ) Map.empty

    let total = 
        numbers
        |> Seq.filter (numberIsValid symbolsMap) 
        |> Seq.map (fun num -> num.value)
        |> Seq.sum
    total

printfn "Answer: %A" answer
