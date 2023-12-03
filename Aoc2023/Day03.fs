module Aoc2023.Day03

open System
open System.Text.RegularExpressions

type Symbol = { symbol: char; offset: int }

type Schematic =
    { width: int
      data: string
      symbols: Symbol list }

let loadSchematic path =
    let lines = IO.File.ReadLines path |> Seq.toList

    let data = lines |> String.concat ""

    let symbols =
        seq {
            for m in Regex.Matches(data, @"[^\w\.]") do
                yield
                    { symbol = m.Value[0]
                      offset = m.Index }
        }
        |> Seq.toList

    { width = lines[0].Length
      data = data
      symbols = symbols }


let adjacents width offset length =
    let col = offset % width
    let startOffset = if col = 0 then offset else offset - 1

    let endOffset =
        if col + length >= width then
            offset + length - 1
        else
            offset + length

    let leftside = [ startOffset .. offset - 1 ]
    let rightside = [ offset + length .. endOffset ]

    let mid =
        seq { for n in startOffset..endOffset -> [ n - width; n + width ] }
        |> Seq.concat
        |> Seq.toList

    Seq.concat [ leftside; mid; rightside ] |> Set.ofSeq


let hasAdjacentSymbolInSchematic schematic (offset, (digits: string)) =
    schematic.symbols
    |> Seq.map (fun s -> s.offset)
    |> Set.ofSeq
    |> Set.intersect (adjacents schematic.width offset digits.Length)
    |> Set.isEmpty
    |> not

let part1 path =
    let schematic = loadSchematic path

    seq {
        for m in Regex.Matches(schematic.data, @"[\d]+") do
            yield m.Index, m.Value
    }
    |> Seq.filter (hasAdjacentSymbolInSchematic schematic)
    |> Seq.sumBy (snd >> Int32.Parse)
    |> box

let part2 _path = box 0

open Xunit

[<Fact>]
let ``day 03 part 1`` () =
    Assert.Equal(box 4361, (part1 (__SOURCE_DIRECTORY__ + "/input/day-03.example")))

[<Fact>]
let ``day 03 part 2`` () =
    Assert.Equal(box 0, (part2 (__SOURCE_DIRECTORY__ + "/input/day-03.example")))


[<Fact>]
let ``adjacents mid`` () =
    let width = 10
    let offset = 11
    let length = 3

    let expected = [ 0; 1; 2; 3; 4; 10; 14; 20; 21; 22; 23; 24 ] |> Set.ofList
    let adjcs = adjacents width offset length
    let unexpecteds = Set.difference adjcs expected
    let missing = Set.difference expected adjcs

    Assert.True(Set.isEmpty unexpecteds, $"Got extra locations {unexpecteds}")
    Assert.True(Set.isEmpty missing, $"Got missing locations {missing}")

[<Fact>]
let ``adjacents right`` () =
    let width = 10
    let offset = 17
    let length = 3

    let expected = [ 6; 7; 8; 9; 16; 26; 27; 28; 29 ] |> Set.ofList
    let adjcs = adjacents width offset length
    let unexpecteds = Set.difference adjcs expected
    let missing = Set.difference expected adjcs

    Assert.True(Set.isEmpty unexpecteds, $"Got extra locations {unexpecteds}")
    Assert.True(Set.isEmpty missing, $"Got missing locations {missing}")
