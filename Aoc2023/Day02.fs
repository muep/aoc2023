module Aoc2023.Day02

open System

type SetOfCubes = { red: int; green: int; blue: int }

let emptySetOfCubes = { red = 0; green = 0; blue = 0 }

let isSubsetOf maxSet cubeSet =
    (maxSet.red >= cubeSet.red
     && maxSet.green >= cubeSet.green
     && maxSet.blue >= cubeSet.blue)

let minumumCommonSet set1 set2 =
    { red = Int32.Max(set1.red, set2.red)
      green = Int32.Max(set1.green, set2.green)
      blue = Int32.Max(set1.blue, set2.blue) }

type Game =
    { id: int
      setsOfCubes: SetOfCubes list }

let splitFirst (sep: string) (s: string) =
    let position = s.IndexOf sep

    if position = -1 then
        s, ""
    else
        s.Substring(0, position), s.Substring(sep.Length + position)

let addToSetOfCubes setOfCubes (cubes: string) =
    let count, colorText =
        match cubes.Split(" ") with
        | [| numText; colorText |] -> Int32.Parse(numText), colorText
        | _ -> invalidArg cubes "Expected just two items separated by space"

    match colorText with
    | "red" -> { setOfCubes with red = count }
    | "green" -> { setOfCubes with green = count }
    | "blue" -> { setOfCubes with blue = count }
    | _ -> invalidArg cubes "Expected to get one of the possible colors"

let loadSetOfCubes (text: string) =
    text.Split(", ") |> Array.fold addToSetOfCubes emptySetOfCubes

let loadGame (gameText: string) =
    let titleText, setsOfCubesText = splitFirst ": " gameText

    let setsOfCubes =
        setsOfCubesText.Split("; ") |> Seq.map loadSetOfCubes |> Seq.toList

    { id = Int32.Parse(titleText.Substring(5))
      setsOfCubes = setsOfCubes }

let maximumSetOfCubes = { red = 12; green = 13; blue = 14 }

let part1 path =
    IO.File.ReadLines path
    |> Seq.map loadGame
    |> Seq.filter ((fun game -> game.setsOfCubes) >> (Seq.forall (isSubsetOf maximumSetOfCubes)))
    |> Seq.map (fun game -> game.id)
    |> Seq.sum
    |> box

let part2 path =
    IO.File.ReadLines path
    |> Seq.map (
        loadGame
        >> (fun game -> game.setsOfCubes)
        >> (Seq.fold minumumCommonSet emptySetOfCubes)
        >> (fun
                { red = red
                  green = green
                  blue = blue } -> red * green * blue)
    )
    |> Seq.sum
    |> box

open Xunit

[<Fact>]
let ``day 02 part 1`` () =
    Assert.Equal(box 8, (part1 (__SOURCE_DIRECTORY__ + "/input/day-02.example")))

[<Fact>]
let ``day 02 part 2`` () =
    Assert.Equal(box 2286, (part2 (__SOURCE_DIRECTORY__ + "/input/day-02.example")))

[<Fact>]
let ``splitFirst basics`` () =
    Assert.Equal(("asd", "basd"), splitFirst ": " "asd: basd")
    Assert.Equal(("asd: basd", ""), splitFirst "=" "asd: basd")

[<Fact>]
let ``loadGame basics`` () =
    let line = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

    let expectedGame =
        { id = 2
          setsOfCubes =
            [ { red = 0; green = 2; blue = 1 }
              { red = 1; green = 3; blue = 4 }
              { red = 0; green = 1; blue = 1 } ] }

    Assert.Equal(expectedGame, loadGame line)
