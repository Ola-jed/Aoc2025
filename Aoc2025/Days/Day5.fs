module Aoc2025.Day5

open Aoc2025.Days

type Day5() =
    let mergeIntervals (intervals: (int64 * int64) array) =
        intervals
        |> Array.sortBy fst
        |> Array.fold
            (fun acc (a, b) ->
                match acc with
                | [] -> [ (a, b) ]
                | (x, y) :: rest -> if a <= y + 1L then (x, max y b) :: rest else (a, b) :: acc)
            []
        |> List.rev


    interface IDay with
        member _.Day = 5

        member this.Part1() =
            let content = Input.readLines (this :> IDay).Day

            let freshIngredientRanges =
                content
                |> Array.filter _.Contains("-")
                |> Array.map _.Split("-")
                |> Array.map (fun x -> (int64 (x[0]), int64 (x[1])))

            content
            |> Array.filter (fun x -> x.Contains("-") = false)
            |> Array.map int64
            |> Array.filter (fun x ->
                let mutable contained = false

                for left, right in freshIngredientRanges do
                    if contained = false && left <= x && x <= right then
                        contained <- true

                contained)
            |> Array.length
            |> string


        member this.Part2() =
            Input.readLines (this :> IDay).Day
            |> Array.filter _.Contains("-")
            |> Array.map _.Split("-")
            |> Array.map (fun x -> (int64 (x[0]), int64 (x[1])))
            |> Array.sortBy fst
            |> mergeIntervals
            |> List.map (fun (x, y) -> 1L + y - x)
            |> List.sum
            |> string
