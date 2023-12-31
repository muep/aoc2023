module Aoc2023.Day06

let notEmpty (a: string) = a.Length > 0

let splitFirst (sep: string) (s: string) =
    match s.IndexOf sep with
    | -1 -> s, ""
    | n -> s.Substring(0, n), s.Substring(sep.Length + n)

let nums s =
    let _, rest = splitFirst ":" s
    rest.Split() |> Seq.filter notEmpty |> Seq.map System.Int32.Parse

let load1 path =
    match System.IO.File.ReadLines path |> Seq.take 2 |> Seq.toList with
    | [ t; d ] -> Seq.zip (nums t) (nums d)
    | _ -> invalidArg path "Expected two lines"

let num s =
    let _, rest = splitFirst ":" s
    rest.Replace(" ", "") |> System.Int64.Parse

let load2 path =
    match System.IO.File.ReadLines path |> Seq.take 2 |> Seq.toList with
    | [ t; d ] -> (num t), (num d)
    | _ -> invalidArg path "Expected two lines"

let surplusDist raceTime recordDist chr =
    -(chr * chr) + (chr * raceTime) - recordDist

let rec lastPositive f startLimit endLimit =
    if System.Int64.Abs(startLimit - endLimit) = 1 then
        startLimit
    else
        let midpoint = (startLimit + endLimit) / 2L

        if f midpoint <= 0L then
            lastPositive f startLimit midpoint
        else
            lastPositive f midpoint endLimit

let numberOfWinningChoices raceTime recordDistance =
    let midpoint = raceTime / 2L
    let f = surplusDist raceTime recordDistance

    if f midpoint <= 0 then
        0L
    else
        let lastWin = lastPositive f midpoint raceTime
        let fstWin = lastPositive f midpoint 0
        lastWin - fstWin + 1L

let part1 path =
    load1 path
    |> Seq.map (fun (t, d) -> numberOfWinningChoices t d)
    |> Seq.reduce (*)
    |> box

let part2 path =
    load2 path |> (fun (t, d) -> numberOfWinningChoices t d) |> box

open Xunit

[<Fact>]
let ``day 06 part 1`` () =
    Assert.Equal(box 288L, (part1 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))

[<Fact>]
let ``day 06 part 2`` () =
    Assert.Equal(box 71503L, (part2 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))

[<Fact>]
let ``numberOfWinningChoices basics`` () =
    Assert.Equal(38L, numberOfWinningChoices 55L 401L)
