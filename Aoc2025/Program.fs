open Aoc2025.Day4
open Aoc2025.Days

[<EntryPoint>]
let main _ =
    let day = Day4() :> IDay
    printfn $"%s{day.Part1()}"
    printfn $"%s{day.Part2()}"

    0
