module Aoc2023.Day00

let part1 (_path: string) = box 0
let part2 (_path: string) = box 0

open Xunit

[<Fact>]
let ``day 00 part 1`` () =
    Assert.Equal((part1 (__SOURCE_DIRECTORY__ + "/input/day-00.example")), 0)

[<Fact>]
let ``day 00 part 2`` () =
    Assert.Equal((part2 (__SOURCE_DIRECTORY__ + "/input/day-00.example")), 0)
