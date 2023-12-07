module Aoc2023.Day06

let part1 _ = box 0
let part2 _ = box 0

open Xunit

[<Fact>]
let ``day 06 part 1`` () =
    Assert.Equal(box 0, (part1 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))

[<Fact>]
let ``day 06 part 2`` () =
    Assert.Equal(box 0, (part2 (__SOURCE_DIRECTORY__ + "/input/day-06.example")))
