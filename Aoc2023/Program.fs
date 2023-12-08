type Solution =
    { part1: string -> obj
      part2: string -> obj }

let solutions =
    [ (1,
       { part1 = Aoc2023.Day01.part1
         part2 = Aoc2023.Day01.part2 })
      (2,
       { part1 = Aoc2023.Day02.part1
         part2 = Aoc2023.Day02.part2 })
      (3,
       { part1 = Aoc2023.Day03.part1
         part2 = Aoc2023.Day03.part2 })
      (4,
       { part1 = Aoc2023.Day04.part1
         part2 = Aoc2023.Day04.part2 })
      (5,
       { part1 = Aoc2023.Day05.part1
         part2 = Aoc2023.Day05.part2 })
      (6,
       { part1 = Aoc2023.Day06.part1
         part2 = Aoc2023.Day06.part2 })
      (7,
       { part1 = Aoc2023.Day07.part1
         part2 = Aoc2023.Day07.part2 }) ]
    |> Map.ofList

let runDay inputPath day =
    let stopwatch = System.Diagnostics.Stopwatch()
    stopwatch.Start()
    let part1Res = solutions.[day].part1 inputPath
    stopwatch.Stop()
    System.Console.WriteLine $"day-%02d{day} part1: {part1Res}  ({stopwatch.ElapsedMilliseconds} ms)"
    stopwatch.Reset()
    stopwatch.Start()
    let part2Res = solutions.[day].part2 inputPath
    stopwatch.Stop()
    System.Console.WriteLine $"day-%02d{day} part2: {part2Res} ({stopwatch.ElapsedMilliseconds} ms)"
    ()

let runDayDefault day = runDay $"input/day-%02d{day}.txt" day

let runAll () =
    runDayDefault 1
    runDayDefault 2
    runDayDefault 3
    runDayDefault 4
    runDayDefault 5
    runDayDefault 6
    runDayDefault 7

let usageText =
    """
usage:
    Aoc2023                this help
    Aoc2023 all            run all the implemented solutions
    Aoc2023 day <N>        run both parts of a daily solution with default input
    Aoc2023 day <N> <path> run both parts of a daily solution with given input
"""

[<EntryPoint>]
let main args =
    match args with
    | [||] -> System.Console.WriteLine usageText
    | [| "all" |] -> runAll ()
    | [| "day"; num |] -> num |> System.Int32.Parse |> runDayDefault
    | [| "day"; num; inputPath |] -> num |> System.Int32.Parse |> runDay inputPath
    | _ -> invalidArg $"%A{args}" "Unexpected command line syntax"

    0
