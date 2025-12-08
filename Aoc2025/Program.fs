open Aoc2025.Day8
open Aoc2025.Days

[<EntryPoint>]
let main _ =
    let day = Day8() :> IDay
    printfn $"%s{day.Part1()}"
    printfn $"%s{day.Part2()}"

    0
