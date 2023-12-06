module Aoc2023.Day05

type foldState =
    { ranges: (uint64 * uint64 * uint64) list
      maps: (uint64 -> uint64) list
      seeds: uint64 seq }

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

let foldRanges (ranges: (uint64 * uint64) list) (start: uint64, len: uint64) =
    match ranges with
    | [] -> [ (start, len) ]
    | (prevStart, prevLen) :: olderRanges ->
        let prevEnd = prevStart + prevLen

        if start <= prevEnd then
            let thisEnd = start + len
            let realEnd = System.UInt64.Max(prevEnd, thisEnd)
            (prevStart, realEnd - prevStart) :: olderRanges
        else
            (start, len) :: (prevStart, prevLen) :: olderRanges

let mergeRanges (ranges: (uint64 * uint64) array) =
    ranges
    |> Array.sort
    |> Seq.fold foldRanges []
    |> Seq.toArray
    |> Array.rev

let part1SeedsFromLine (line: string) =
    line.Substring(6).Split()
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map System.UInt64.Parse
    |> Array.toSeq

let part2SeedsFromLine (line: string) =
    line.Substring(6).Split()
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map System.UInt64.Parse
    |> Array.chunkBySize 2
    |> Array.map (fun chunk ->
        match chunk with
        | [| start; len |] -> start, len
        | _ -> 0UL, 0UL)
    |> mergeRanges
    |> Array.map (fun (start, len) -> seq { for n in start .. (start + len - 1UL) -> n })
    |> Seq.concat

let foldLine seedsFromLine state (line: string) =
    match state, line with
    // Initial line
    | { seeds = seeds }, line when Seq.isEmpty seeds ->
        { state with
            seeds = seedsFromLine line }
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
        |> Seq.fold (foldLine part1SeedsFromLine) initialState
        |> (fun { maps = maps; seeds = seeds } -> seeds, Seq.reduce (>>) (List.rev maps))

    seeds |> Seq.map mapping |> Seq.min |> box

let part2 path =
    let seeds, mapping =
        Seq.concat [ System.IO.File.ReadLines path; [ "" ] ]
        |> Seq.fold (foldLine part2SeedsFromLine) initialState
        |> (fun { maps = maps; seeds = seeds } -> seeds, Seq.reduce (>>) (List.rev maps))

    seeds |> Seq.map mapping |> Seq.min |> box

open Xunit

[<Fact>]
let ``day 05 part 1`` () =
    Assert.Equal(box 35UL, (part1 (__SOURCE_DIRECTORY__ + "/input/day-05.example")))

[<Fact>]
let ``day 05 part 2`` () =
    Assert.Equal(box 46UL, (part2 (__SOURCE_DIRECTORY__ + "/input/day-05.example")))

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

[<Fact>]
let ``mergeRanges basic separate`` () =
    let input = [|(2UL, 2UL);(5UL, 2UL)|]
    let expected = [|(2UL, 2UL);(5UL, 2UL)|] |> Seq.toList
    let actual = mergeRanges input |> Seq.toList
    Assert.True(actual.Equals(expected), $"Expected {expected}, got {actual}")

[<Fact>]
let ``mergeRanges basic sans overlap`` () =
    let input = [|(0UL, 5UL);(5UL, 5UL)|]
    let expected = [|(0UL, 10UL)|] |> Seq.toList
    let actual = mergeRanges input |> Seq.toList
    Assert.True(actual.Equals(expected), $"Expected {expected}, got {actual}")

[<Fact>]
let ``mergeRanges basic with overlap`` () =
    let input = [|(0UL, 5UL);(4UL, 5UL)|]
    let expected = [|(0UL, 9UL)|] |> Seq.toList
    let actual = mergeRanges input |> Seq.toList
    Assert.True(actual.Equals(expected), $"Expected {expected}, got {actual}")

[<Fact>]
let ``mergeRanges basic with overlap - different offset`` () =
    let input = [|(1UL, 5UL);(5UL, 5UL)|]
    let expected = [|(1UL, 9UL)|] |> Seq.toList
    let actual = mergeRanges input |> Seq.toList
    Assert.True(actual.Equals(expected), $"Expected {expected}, got {actual}")
