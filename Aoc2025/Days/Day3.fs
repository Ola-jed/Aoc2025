module Aoc2025.Day3

open System
open Aoc2025.Days

type Day3() =
    static member private HighestJoltage(arr: int array) =
        arr
        |> Array.rev
        |> Array.fold (fun (maxTail, best) x ->
            let best = max best (x * 10 + maxTail)
            max (x, best) (maxTail, best)
        ) (0, 0)
        |> snd


    interface IDay with
        member this.Day = 3

        member this.Part1() =
            Input.readLines (this :> IDay).Day
            |> Array.map _.ToCharArray()
            |> Array.map (Array.map (fun c -> int c - int '0'))
            |> Array.map Day3.HighestJoltage
            |> Array.sum
            |> string


        member this.Part2() = ""
