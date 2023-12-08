module Aoc2023.Day07

type C = C of int

let loadCard (c: char) =
    match c with
    | '2' -> C 2
    | '3' -> C 3
    | '4' -> C 4
    | '5' -> C 5
    | '6' -> C 6
    | '7' -> C 7
    | '8' -> C 8
    | '9' -> C 9
    | 'T' -> C 10
    | 'J' -> C 11
    | 'Q' -> C 12
    | 'K' -> C 13
    | 'A' -> C 14
    | _ -> invalidArg "c" $"non-card character {c}"

let distinct things =
    things
    |> Seq.fold
        (fun counts thing ->
            match Map.tryFind thing counts with
            | Some a -> Map.add thing (a + 1) counts
            | None -> Map.add thing 1 counts)
        Map.empty
    |> Map.toList
    |> List.sortBy (fun (thing, cnt) -> cnt, thing)
    |> List.rev

type Hand =
    | HighCard of C array
    | OnePair of C array
    | TwoPair of C array
    | ThreeOfAKind of C array
    | FullHouse of C array
    | FourOfAKind of C array
    | FiveOfAKind of C array

module Hand =
    let ofCards cards =
        let cardsByCount = distinct cards

        match cardsByCount with
        | (_, 5) :: _ -> FiveOfAKind cards
        | (_, 4) :: _ -> FourOfAKind cards
        | (_, 3) :: (_, 2) :: _ -> FullHouse cards
        | (_, 3) :: _ -> ThreeOfAKind cards
        | (_, 2) :: (_, 2) :: _ -> TwoPair cards
        | (_, 2) :: _ -> OnePair cards
        | _ -> HighCard cards

let loadHand (cardText: string) =
    [| for c in cardText -> loadCard c |] |> Hand.ofCards

let load path =
    System.IO.File.ReadLines path
    |> Seq.map (fun line ->
        match line.Split() with
        | [| cards; bid |] -> loadHand cards, System.UInt64.Parse bid
        | a -> invalidArg "line" $"Expected two items, got {a.Length} of them")

let part1 path =
    load path
    |> Seq.toList
    |> List.sortBy fst
    |> Seq.indexed
    |> Seq.map (fun (rankMinusOne, t) -> uint64 (rankMinusOne) + 1UL, t)
    |> Seq.map (fun (rank, (_, bid)) -> rank * bid)
    |> Seq.sum
    |> box

let part2 _ = box 0

open Xunit

[<Fact>]
let ``day 07 part 1`` () =
    Assert.Equal(box 6440UL, (part1 (__SOURCE_DIRECTORY__ + "/input/day-07.example")))

[<Fact>]
let ``day 07 part 2`` () =
    Assert.Equal(box 0, (part2 (__SOURCE_DIRECTORY__ + "/input/day-07.example")))
