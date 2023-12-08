module Aoc2023.Day07

type Card = Card of int

module Card =
    let load (c: char) =
        match c with
        | '2' -> Card 2
        | '3' -> Card 3
        | '4' -> Card 4
        | '5' -> Card 5
        | '6' -> Card 6
        | '7' -> Card 7
        | '8' -> Card 8
        | '9' -> Card 9
        | 'T' -> Card 10
        | 'J' -> Card 11
        | 'Q' -> Card 12
        | 'K' -> Card 13
        | 'A' -> Card 14
        | _ -> invalidArg "c" $"non-card character {c}"

    let addJokers card =
        match card with
        | Card 11 -> Card 1
        | c -> c

    let isJoker card =
        match card with
        | Card 1 -> true
        | _ -> false

type Hand =
    | HighCard of Card array
    | OnePair of Card array
    | TwoPair of Card array
    | ThreeOfAKind of Card array
    | FullHouse of Card array
    | FourOfAKind of Card array
    | FiveOfAKind of Card array

module Hand =
    let basicEval cards =
        let cardsByCount = Seq.countBy id cards |> List.ofSeq |> List.sortByDescending snd

        match cardsByCount with
        | (_, 5) :: _ -> FiveOfAKind cards
        | (_, 4) :: _ -> FourOfAKind cards
        | (_, 3) :: (_, 2) :: _ -> FullHouse cards
        | (_, 3) :: _ -> ThreeOfAKind cards
        | (_, 2) :: (_, 2) :: _ -> TwoPair cards
        | (_, 2) :: _ -> OnePair cards
        | _ -> HighCard cards

    let jokerEval cards =
        let jokers, normalCards = Array.partition Card.isJoker cards

        let cardsByCount =
            Seq.countBy id normalCards |> List.ofSeq |> List.sortByDescending snd

        match jokers.Length, cardsByCount with
        | 5, _ -> FiveOfAKind cards
        | jc, (_, n) :: _ when n + jc >= 5 -> FiveOfAKind cards

        | jc, (_, n) :: _ when n + jc >= 4 -> FourOfAKind cards

        | 0, [ (_, 3); (_, 2) ] -> FullHouse cards
        | 1, [ (_, 3); (_, 1) ] -> FullHouse cards
        | 1, [ (_, 2); (_, 2) ] -> FullHouse cards

        | jc, (_, n) :: _ when n + jc >= 3 -> ThreeOfAKind cards

        | 0, (_, 2) :: (_, 2) :: _ -> TwoPair cards
        | jc, (_, n) :: _ when n + jc >= 2 -> OnePair cards
        | _ -> HighCard cards

    let load cardRule evalHand (cardText: string) =
        [| for c in cardText -> Card.load c |] |> Array.map cardRule |> evalHand

let load cardRule evalHand path =
    System.IO.File.ReadLines path
    |> Seq.map (fun line ->
        match line.Split() with
        | [| cardText; bid |] -> Hand.load cardRule evalHand cardText, System.UInt64.Parse bid
        | a -> invalidArg "line" $"Expected two items, got {a.Length} of them")

let part cardRule evalRules path =
    load cardRule evalRules path
    |> Seq.toList
    |> List.sortBy fst
    |> Seq.indexed
    |> Seq.map (fun (rankMinusOne, t) -> uint64 (rankMinusOne) + 1UL, t)
    |> Seq.map (fun a ->
        printfn $"{a}"
        a)
    |> Seq.map (fun (rank, (_, bid)) -> rank * bid)
    |> Seq.sum
    |> box

let part1 = part id Hand.basicEval

let part2 = part Card.addJokers Hand.jokerEval

open Xunit

[<Fact>]
let ``day 07 part 1`` () =
    Assert.Equal(box 6440UL, (part1 (__SOURCE_DIRECTORY__ + "/input/day-07.example")))

[<Fact>]
let ``day 07 part 2`` () =
    Assert.Equal(box 5905UL, (part2 (__SOURCE_DIRECTORY__ + "/input/day-07.example")))
