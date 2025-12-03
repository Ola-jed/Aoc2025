module Aoc2025.Day3

open Aoc2025.Days

type Day3() =
    static member private HighestJoltage (arr: int64 array) depth =
        let n = arr.Length
        let mutable startIndex = 0
        let mutable remaining = depth
        let mutable acc = 0L

        while remaining > 0 do
            let lastSearchIndex = n - remaining
            let mutable bestValue = -1L
            let mutable bestIdx = startIndex

            for i in startIndex..lastSearchIndex do
                if arr[i] > bestValue then
                    bestValue <- arr[i]
                    bestIdx <- i

            acc <- acc * 10L + bestValue
            remaining <- remaining - 1
            startIndex <- bestIdx + 1

        acc

    static member private Highest2 arr = Day3.HighestJoltage arr 2

    static member private Highest12 arr = Day3.HighestJoltage arr 12

    interface IDay with
        member _.Day = 3

        member this.Part1() =
            Input.readLines (this :> IDay).Day
            |> Array.map (_.ToCharArray() >> Array.map (fun c -> int64 c - int64 '0'))
            |> Array.map Day3.Highest2
            |> Array.sum
            |> string

        member this.Part2() =
            Input.readLines (this :> IDay).Day
            |> Array.map (_.ToCharArray() >> Array.map (fun c -> int64 c - int64 '0'))
            |> Array.map Day3.Highest12
            |> Array.sum
            |> string
