open Aoc2025.Day2
open Aoc2025.Days

[<EntryPoint>]
let main _ =
    let day2 = Day2() :> IDay
    printfn $"%s{day2.Part1()}"
    printfn $"%s{day2.Part2()}"

    0
