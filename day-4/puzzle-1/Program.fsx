open System.IO
open System.Text.RegularExpressions

type Card = {
    cardNumber: int
    winningNumbers: int list
    numbers: int list
}

type LeftOrRight = Left | Right

let lineToCard (line: string) = 
    // Some helper functions so we don't have to write ugly C# style code
    let replace (findStr: string) (replaceStr: string) (str: string) = str.Replace(findStr, replaceStr)
    let split (by: string) (str: string) = str.Split(by)
    let splitByAndTake (splitBy: string) leftOrRight (str: string) =
        let index = match leftOrRight with | Left -> 0 | Right -> 1
        (str |> split splitBy).[index].Trim()
    let convertToInt str = int str 

    let cardNumber = 
        line 
        |> splitByAndTake ":" Left 
        |> replace " " "" 
        |> replace "Card" "" 
        |> int

    let winningNumbers = 
        line 
        |> splitByAndTake ":" Right 
        |> splitByAndTake "|" Left 
        |> replace "  " " " 
        |> split " "
        |> List.ofArray
        |> List.map convertToInt

    let numbers = 
        line 
        |> splitByAndTake ":" Right 
        |> splitByAndTake "|" Right 
        |> replace "  " " " 
        |> split " "
        |> List.ofArray
        |> List.map convertToInt

    { cardNumber = cardNumber; winningNumbers = winningNumbers; numbers = numbers }

let numberOfWinningNumbersFromCard (card: Card) =
    let intersection = Set.intersect (card.numbers |> Set.ofList) (card.winningNumbers |> Set.ofList)
    intersection.Count

let rec doubleTimes (number: int) (times: int) = 
    match times with 
    | 0 -> 0 
    | 1 -> number
    | _ -> doubleTimes (number * 2) (times - 1)

let doubleStartingAt1Times (times: int) = doubleTimes 1 times


let answer =
    let lines = 
        Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
        |> Path.GetFullPath
        |> File.ReadAllLines
    lines
        |> Seq.map lineToCard
        |> Seq.map numberOfWinningNumbersFromCard
        |> Seq.map doubleStartingAt1Times
        |> Seq.sum

printfn "Answer: %A" answer
