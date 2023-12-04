open System.IO
open System.Text.RegularExpressions

type Card = {
    cardNumber: int
    winningNumbers: int list
    numbers: int list
}

type Tree<'T> = {
    value: 'T
    children: Tree<'T> list
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

let rec cascade (cards: Card list) (indexList: int list) = 
    match indexList with
    // We'll set up this condition when no additional paths available
    | [-1] -> 0
    | _ ->
        let nextList = 
            // Loop through all of the cards each time, checking if the current index is in our list
            cards
            |> List.mapi (fun i card -> 
                // Find all occurences of the matching card index
                let occurrences = indexList |> List.filter (fun index -> index = i)
                if occurrences.Length > 0 then
                    // Get the number of winners for the current card
                    let numberOfWinners = numberOfWinningNumbersFromCard card
                    // Get the index range for the winning cards underneath the current card
                    let winnerRange = [(i + 1)..(i + numberOfWinners)]
                    // And duplicate that range for as many occurrences we had in our index list
                    winnerRange |> List.replicate occurrences.Length |> List.collect id
                else
                    // Just flag that no occurrences happened, we'll filter this out
                    [-1]
            )
            |> List.collect id //Smoosh all of the lists together
            |> List.filter (fun i -> i <> -1)
        // Recurse again adding the length of the list of indexes we just generated
        (cascade cards (if nextList.Length = 0 then [-1] else nextList)) + nextList.Length

let getAllCardCopies (cards: Card list) =
    // Run the Recursive function
    let allButFirstLayer = cascade cards [0..(cards.Length - 1)]
    // Make sure to add the initial layer of cards (205 of them)
    allButFirstLayer + cards.Length

let answer =
    let lines = 
        Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt") 
        |> Path.GetFullPath
        |> File.ReadAllLines
    let cards = 
        lines
        |> List.ofArray
        |> List.map lineToCard
    getAllCardCopies cards

printfn "Answer: %A" answer
