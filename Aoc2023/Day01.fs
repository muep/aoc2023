module Aoc2023.Day01

open System

let calibrationValueFromLine line =
    line
    |> Seq.filter Char.IsDigit
    |> (fun s -> (Seq.head s, Seq.last s))
    |> (fun (n0, n1) -> $"{n0}{n1}")
    |> UInt32.Parse

let digits =
    [ ("0", 0)
      ("1", 1)
      ("one", 1)
      ("2", 2)
      ("two", 2)
      ("3", 3)
      ("three", 3)
      ("4", 4)
      ("four", 4)
      ("5", 5)
      ("five", 5)
      ("6", 6)
      ("six", 6)
      ("7", 7)
      ("seven", 7)
      ("8", 8)
      ("eight", 8)
      ("9", 9)
      ("nine", 9) ]

let digitFromStringStart (s: string) =
    digits
    |> Seq.filter (fun (nom, _) -> (s.StartsWith nom))
    |> Seq.tryHead
    |> Option.map snd

let digitFromStringEnd (s: string) =
    digits
    |> Seq.filter (fun (nom, _) -> (s.EndsWith nom))
    |> Seq.tryHead
    |> Option.map snd

let firstDigit (s: string) =
    seq { for i in 0 .. s.Length -> i }
    |> Seq.map s.Substring
    |> Seq.choose digitFromStringStart
    |> Seq.head

let lastDigit (s: string) =
    seq { for i in 0 .. s.Length -> s.Length - i }
    |> Seq.map (fun skip -> (s.Substring(0, skip)))
    |> Seq.choose digitFromStringEnd
    |> Seq.head

let firstAndLastDigit (s: string) = (firstDigit s), (lastDigit s)

let calibrationValueFromLinePart2 line =
    line |> firstAndLastDigit |> (fun (n0, n1) -> $"{n0}{n1}") |> UInt32.Parse

let part1 path =
    IO.File.ReadLines path |> Seq.map calibrationValueFromLine |> Seq.sum |> box

let part2 path =
    IO.File.ReadLines path
    |> Seq.map calibrationValueFromLinePart2
    |> Seq.sum
    |> box


open Xunit

[<Fact>]
let ``day 01 part 1`` () =
    Assert.Equal(box 142u, (part1 (__SOURCE_DIRECTORY__ + "/input/day-01.example")))

[<Fact>]
let ``day 01 part 2`` () =
    Assert.Equal(box 281u, (part2 (__SOURCE_DIRECTORY__ + "/input/day-01-part2.example")))

[<Fact>]
let ``day 01 part 2 lastDigit`` () = Assert.Equal(9, lastDigit "two1nine")

[<Fact>]
let ``day 01 part 2 bits`` () =
    Assert.Equal(29u, calibrationValueFromLinePart2 "two1nine")
    Assert.Equal(83u, calibrationValueFromLinePart2 "eightwothree")
    Assert.Equal(13u, calibrationValueFromLinePart2 "abcone2threexyz")
    Assert.Equal(24u, calibrationValueFromLinePart2 "xtwone3four")
    Assert.Equal(42u, calibrationValueFromLinePart2 "4nineeightseven2")
    Assert.Equal(14u, calibrationValueFromLinePart2 "zoneight234")
    Assert.Equal(76u, calibrationValueFromLinePart2 "7pqrstsixteen")
