module Aoc2023.Day04

open System

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

let scoreFromCount count =
    match count with
    | 0 -> 0
    | n -> 1 <<< (n - 1)

let part1 path =
    IO.File.ReadLines path
    |> Seq.map (splitFirst ": " >> snd >> trim >> getWinningAndWhatWeHave)
    |> Seq.map (fun (a, b) -> Set.intersect a b |> Set.count)
    |> Seq.map scoreFromCount
    |> Seq.sum
    |> box

open Xunit

[<Fact>]
let ``day 04 part 1`` () =
    Assert.Equal(box 13, (part1 (__SOURCE_DIRECTORY__ + "/input/day-04.example")))
