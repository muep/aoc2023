module Aoc2023.Day05

type foldState =
    { ranges: (uint64 * uint64 * uint64) list
      maps: (uint64 -> uint64) list
      seeds: uint64 list }

let initialState = { ranges = []; maps = []; seeds = [] }

let isInSrcRange targetSrc (_, src, len) =
    src <= targetSrc && targetSrc < src + len

let funFromRanges ranges =
    (fun src ->
        match ranges |> List.filter (isInSrcRange src) |> List.tryHead with
        | None -> src
        | Some((rngDst, rngSrc, _)) -> src + rngDst - rngSrc)

let rangeFromLine (line: string) =
    match line.Split() with
    | [| dst; src; len |] -> (System.UInt64.Parse dst), (System.UInt64.Parse src), (System.UInt64.Parse len)
    | _ -> invalidArg line "Expected three numbers"

let foldLine state (line: string) =
    match state, line with
    // Initial line
    | { seeds = [] }, line ->
        let newSeeds =
            line.Substring(6).Split()
            |> Array.filter (fun s -> s.Length > 0)
            |> Array.map System.UInt64.Parse
            |> Array.toList

        { state with seeds = newSeeds }
    // Not yet collecting the names, so they are just ignored
    | { ranges = [] }, line when line.EndsWith("map:") -> state
    // Empty line would otherwise be interesting, but not if nothing
    // has been collected
    | { ranges = [] }, "" -> state
    // Finally a case where something has been accumulated and
    // there is a gap
    | { ranges = ranges; maps = maps }, "" ->
        { state with
            ranges = []
            maps = (funFromRanges ranges) :: maps }
    // The actual payload lines
    | { ranges = ranges }, line ->
        { state with
            ranges = (rangeFromLine line) :: ranges }

let part1 path =
    let seeds, mapping =
        Seq.concat [ System.IO.File.ReadLines path; [ "" ] ]
        |> Seq.fold foldLine initialState
        |> (fun { maps = maps; seeds = seeds } -> seeds, Seq.reduce (>>) (List.rev maps))

    seeds |> Seq.map mapping |> Seq.min |> box

let part2 _ = box 0

open Xunit

[<Fact>]
let ``day 05 part 1`` () =
    Assert.Equal(box 35UL, (part1 (__SOURCE_DIRECTORY__ + "/input/day-05.example")))

[<Fact>]
let ``day 05 part 2`` () =
    Assert.Equal(box 0, (part2 (__SOURCE_DIRECTORY__ + "/input/day-05.example")))

[<Fact>]
let ``funFronRanges basics`` () =
    let ranges = [ (50UL, 98UL, 2UL); (52UL, 50UL, 48UL) ]
    let fn = funFromRanges ranges
    Assert.Equal(0UL, fn 0UL)
    Assert.Equal(1UL, fn 1UL)

    Assert.Equal(48UL, fn 48UL)
    Assert.Equal(49UL, fn 49UL)
    Assert.Equal(52UL, fn 50UL)
    Assert.Equal(53UL, fn 51UL)

    Assert.Equal(98UL, fn 96UL)
    Assert.Equal(99UL, fn 97UL)

    Assert.Equal(50UL, fn 98UL)
    Assert.Equal(51UL, fn 99UL)
