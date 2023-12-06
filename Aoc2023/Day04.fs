module Aoc2023.Day04

open System

type Card =
    { id: int
      winningNumbers: Set<int>
      ourNumbers: Set<int> }

let splitFirst (sep: string) (s: string) =
    match s.IndexOf sep with
    | -1 -> s, ""
    | n -> s.Substring(0, n), s.Substring(sep.Length + n)

let trim (s: string) = s.Trim()

let nums (s: string) =
    s.Trim().Split()
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map Int32.Parse
    |> Set.ofSeq

let getWinningAndWhatWeHave (s: string) =
    match s.Split(" | ") with
    | [| winning; got |] -> (nums winning), (nums got)
    | _ -> invalidArg "s" "Expected to have a single pipe"

let cardFromLine line =
    let carIdText, numberText = splitFirst ": " line

    let cardId = carIdText.Substring(5) |> Int32.Parse
    let winningNumbers, ournumbers =
        numberText
        |> trim
        |> getWinningAndWhatWeHave

    { id = cardId
      winningNumbers = winningNumbers
      ourNumbers = ournumbers }

let scoreFromCount count =
    match count with
    | 0 -> 0
    | n -> 1 <<< (n - 1)

let matchingNumbers { ourNumbers = a; winningNumbers = b } =
    Set.intersect a b |> Set.count

let part1 path =
    IO.File.ReadLines path
    |> Seq.map cardFromLine
    |> Seq.map matchingNumbers
    |> Seq.map scoreFromCount
    |> Seq.sum
    |> box

type CardNode =
    { id: int
      breadth: int }

type State =
    { cardScores: Map<int, int>
      nodeQueue: CardNode seq }

let children { id = id; breadth = breadth } =
    [ for n in id + 1 .. id + breadth -> n ]

let updateScores (oldsum: int, oldScores: Map<int, int>) currentNode =
    currentNode
    |> children
    |> List.map (fun childId -> (Map.find childId oldScores))
    |> List.sum
    |> (+) 1
    |> (fun score -> oldsum + score, (Map.add currentNode.id score oldScores))

let part2 path =
    IO.File.ReadLines path
    |> Seq.map cardFromLine
    |> Seq.map (fun card -> { id = card.id
                              breadth = matchingNumbers card })
    |> Seq.toList
    |> List.rev
    |> Seq.fold updateScores (0, Map.empty)
    |> fst
    |> box

open Xunit

[<Fact>]
let ``day 04 part 1`` () =
    Assert.Equal(box 13, (part1 (__SOURCE_DIRECTORY__ + "/input/day-04.example")))

[<Fact>]
let ``day 04 part 2`` () =
    Assert.Equal(box 30, (part2 (__SOURCE_DIRECTORY__ + "/input/day-04.example")))
