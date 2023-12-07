module Aoc2023.Day06

let notEmpty (a: string) = a.Length > 0

let splitFirst (sep: string) (s: string) =
    match s.IndexOf sep with
    | -1 -> s, ""
    | n -> s.Substring(0, n), s.Substring(sep.Length + n)

let nums s =
    let _, rest = splitFirst ":" s
    rest.Split() |> Seq.filter notEmpty |> Seq.map System.Int32.Parse

let load path =
    match System.IO.File.ReadLines path |> Seq.take 2 |> Seq.toList with
    | [ t; d ] -> Seq.zip (nums t) (nums d)
    | _ -> invalidArg path "Expected two lines"

let dist time chr = chr * (max 0 (time - chr))

let numberOfWinningChoices raceTime recordDistance =
    seq { for chr in 1 .. (raceTime - 1) -> chr }
        |> Seq.filter (fun chr -> (dist raceTime chr) > recordDistance)
        |> Seq.length

let part1 path =
    load path
    |> Seq.map (fun (t, d) -> numberOfWinningChoices t d)
    |> Seq.reduce (*)
    |> box

let part2 _ = box 0

open Xunit

[<Fact>]
let ``day 06 part 1`` () =
    Assert.Equal(box 288, (part1 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))

[<Fact>]
let ``day 06 part 2`` () =
    Assert.Equal(box 0, (part2 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))
