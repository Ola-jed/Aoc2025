module Aoc2025.Day8

open System.Linq
open Aoc2025.Days

type JunctionBox = { X: int64; Y: int64; Z: int64 }
type Circuit = JunctionBox list

type Day8() =
    let distance a b =
        let inline sq x = x * x
        sq (a.X - b.X) + sq (a.Y - b.Y) + sq (a.Z - b.Z)

    interface IDay with
        member _.Day = 8

        member this.Part1() =
            let boxes =
                Input.readLines (this :> IDay).Day
                |> Array.map (fun line ->
                    let parts = line.Split(',')

                    { X = int64 parts[0]
                      Y = int64 parts[1]
                      Z = int64 parts[2] })

            let pairsWithDistance =
                [ for i in 0 .. boxes.Length - 2 do
                      for j in i + 1 .. boxes.Length - 1 do
                          let a, b = boxes[i], boxes[j]
                          yield a, b, distance a b ]
                |> List.sortBy (fun (_, _, d) -> d)
                |> List.toArray

            let circuits =
                boxes
                |> Array.map (fun b -> b, -1)
                |> dict
                |> System.Collections.Generic.Dictionary

            let mutable counter = 0

            for i in 1..999 do
                let x, y, _ = pairsWithDistance[i]
                let cx, cy = circuits[x], circuits[y]

                match cx, cy with
                | -1, -1 ->
                    counter <- counter + 1
                    circuits[x] <- counter
                    circuits[y] <- counter

                | -1, cy -> circuits[x] <- cy

                | cx, -1 -> circuits[y] <- cx

                | cx, cy ->
                    counter <- counter + 1

                    for KeyValue(k, v) in circuits do
                        if v = cx || v = cy then
                            circuits[k] <- counter

                    circuits[x] <- counter
                    circuits[y] <- counter

            circuits
            |> Seq.choose (fun kv -> if kv.Value <> -1 then Some kv.Value else None)
            |> Seq.countBy id
            |> Seq.map snd
            |> Seq.sortDescending
            |> Seq.truncate 3
            |> Seq.map int64
            |> Seq.reduce (*)
            |> string

        // Runs very poorly
        member this.Part2() =
            let boxes =
                Input.readLines (this :> IDay).Day
                |> Array.map (fun line ->
                    let parts = line.Split(',')

                    { X = int64 parts[0]
                      Y = int64 parts[1]
                      Z = int64 parts[2] })

            let pairsWithDistance =
                [ for i in 0 .. boxes.Length - 2 do
                      for j in i + 1 .. boxes.Length - 1 do
                          let a, b = boxes[i], boxes[j]
                          yield a, b, distance a b ]
                |> List.sortBy (fun (_, _, d) -> d)
                |> List.toArray

            let circuits =
                boxes
                |> Array.map (fun b -> b, -1)
                |> dict
                |> System.Collections.Generic.Dictionary

            let mutable counter = 0
            let mutable res = 0L

            for i in 0 .. (pairsWithDistance.Length - 1) do
                let x, y, _ = pairsWithDistance[i]
                let cx, cy = circuits[x], circuits[y]

                match cx, cy with
                | -1, -1 ->
                    counter <- counter + 1
                    circuits[x] <- counter
                    circuits[y] <- counter

                | -1, cy -> circuits[x] <- cy

                | cx, -1 -> circuits[y] <- cx

                | cx, cy ->
                    counter <- counter + 1

                    for KeyValue(k, v) in circuits do
                        if v = cx || v = cy then
                            circuits[k] <- counter

                    circuits[x] <- counter
                    circuits[y] <- counter

                if circuits.Values.ToHashSet().Count = 1 && res = 0 then
                    res <- x.X * y.X

            res |> string
