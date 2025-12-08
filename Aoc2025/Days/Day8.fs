module Aoc2025.Day8

open Aoc2025.Days

type JunctionBox = { X: int; Y: int; Z: int }

type Circuit = JunctionBox list

type Day8() =
    let Distance x y =
        (x.X - y.X) * (x.X - y.X)
        + (x.Y - y.Y) * (x.Y - y.Y)
        + (x.Z - y.Z) * (x.Z - y.Z)

    let isInCircuit (box: JunctionBox) (circuits: JunctionBox list list) =
        circuits |> List.filter (List.contains box)


    interface IDay with
        member _.Day = 8

        member this.Part1() =
            let boxes =
                Input.readLines (this :> IDay).Day
                |> Array.map _.Split(",")
                |> Array.map (fun x ->
                    { X = int (x[0])
                      Y = int (x[1])
                      Z = int (x[2]) })

            let circuits = []
            let pairsWithDistance =
                [ for i in 0 .. boxes.Length - 1 do
                      for j in i + 1 .. boxes.Length - 1 do
                          let a = boxes[i]
                          let b = boxes[j]
                          let d = Distance a b
                          yield (a, b, d) ]
                |> List.sortBy (fun (_, _, d) -> d)


            ""


        member this.Part2() = ""
