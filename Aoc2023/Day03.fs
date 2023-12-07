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
        [ for n in startOffset..endOffset -> [ n - width; n + width ] ] |> List.concat

    List.concat [ leftside; mid; rightside ]

let intersectListWithSet setThings listThings =
    List.filter (fun t -> Set.contains t setThings) listThings

let hasAdjacentSymbolInSchematic symbolPositions width (offset, (digits: string)) =
    adjacents width offset digits.Length
    |> intersectListWithSet symbolPositions
    |> List.isEmpty
    |> not

let part1 path =
    let schematic = loadSchematic path

    let symbolPositions = schematic.symbols |> Seq.map (fun s -> s.offset) |> Set.ofSeq

    seq {
        for m in Regex.Matches(schematic.data, @"[\d]+") do
            yield m.Index, m.Value
    }
    |> Seq.filter (hasAdjacentSymbolInSchematic symbolPositions schematic.width)
    |> Seq.sumBy (snd >> Int32.Parse)
    |> box

let findAdjacentNumbersByLocation schematic =
    seq {
        for m in Regex.Matches(schematic.data, @"[\d]+") do
            yield m.Index, m.Value
    }
    |> Seq.map (fun (offset, digits) ->
        let neighbors = adjacents schematic.width offset digits.Length
        [ for neighbor in neighbors -> neighbor, (offset, digits) ])
    |> Seq.concat
    |> Seq.fold
        (fun m (neighborOffset, (numOffset, digits)) ->
            let olds =
                match Map.tryFind neighborOffset m with
                | Some nums -> nums
                | _ -> []

            let news = List.append olds [ (numOffset, digits) ]
            m.Add(neighborOffset, news))
        Map.empty

let part2 path =
    let schematic = loadSchematic path
    let numbersByLocation = findAdjacentNumbersByLocation schematic

    schematic.symbols
    |> Seq.filter (fun { symbol = s } -> s = '*')
    |> Seq.choose (fun { offset = symbolOffset } ->
        match Map.tryFind symbolOffset numbersByLocation with
        | Some([ (_, num1); (_, num2) ]) -> Some(Int32.Parse(num1) * Int32.Parse(num2))
        | _ -> None)
    |> Seq.sum
    |> box

open Xunit

[<Fact>]
let ``day 03 part 1`` () =
    Assert.Equal(box 4361, (part1 (__SOURCE_DIRECTORY__ + "/input/day-03.example")))

[<Fact>]
let ``day 03 part 2`` () =
    Assert.Equal(box 467835, (part2 (__SOURCE_DIRECTORY__ + "/input/day-03.example")))

[<Fact>]
let ``adjacents mid`` () =
    let width = 10
    let offset = 11
    let length = 3

    let expected = [ 0; 1; 2; 3; 4; 10; 14; 20; 21; 22; 23; 24 ] |> Set.ofList
    let adjcs = adjacents width offset length |> Set.ofSeq
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
    let adjcs = adjacents width offset length |> Set.ofSeq
    let unexpecteds = Set.difference adjcs expected
    let missing = Set.difference expected adjcs

    Assert.True(Set.isEmpty unexpecteds, $"Got extra locations {unexpecteds}")
    Assert.True(Set.isEmpty missing, $"Got missing locations {missing}")
