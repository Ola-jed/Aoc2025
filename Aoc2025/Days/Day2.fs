module Aoc2025.Day2

open Aoc2025.Days

type Day2() =
    static member private IsInvalidIdV1(str: string) =
        str.Length % 2 = 0 && str[.. (str.Length / 2 - 1)] = str[str.Length / 2 ..]

    // https://stackoverflow.com/questions/55823298/how-do-i-check-if-a-string-is-entirely-made-of-the-same-substring
    static member private IsInvalidIdV2(str: string) =
        (str + str).IndexOf(str, 1) <> str.Length

    interface IDay with
        member this.Day = 2

        member this.Part1() =
            Input.getInput (this :> IDay).Day
            |> _.Split(",")
            |> Array.map _.Trim()
            |> Array.filter ((<>) "")
            |> Array.collect (fun x ->
                let parts = x.Split("-")
                let left = int64 parts[0]
                let right = int64 parts[1]
                let len = int (right - left + 1L)
                Array.init len (fun i -> left + int64 i))
            |> Array.map int64
            |> Array.filter (fun x -> x |> string |> Day2.IsInvalidIdV1)
            |> Array.sum
            |> string


        member this.Part2() =
            Input.getInput (this :> IDay).Day
            |> _.Split(",")
            |> Array.map _.Trim()
            |> Array.filter ((<>) "")
            |> Array.collect (fun x ->
                let parts = x.Split("-")
                let left = int64 parts[0]
                let right = int64 parts[1]
                let len = int (right - left + 1L)
                Array.init len (fun i -> left + int64 i))
            |> Array.map int64
            |> Array.filter (fun x -> x |> string |> Day2.IsInvalidIdV2)
            |> Array.sum
            |> string
