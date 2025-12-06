module Aoc2025.Day6

open System.Linq
open Aoc2025.Days

type Item =
    | Value of int64
    | StrValue of string
    | Operator of char

type Day6() =
    let Transpose (matrix: 'a array array) =
        if matrix.Length = 0 then
            [||]
        else
            let rows = matrix.Length
            let cols = matrix[0].Length
            Array.init cols (fun c -> Array.init rows (fun r -> matrix[r][c]))

    let ItemToInt item =
        match item with
        | Value v -> v
        | _ -> 0L


    let ItemToStr item =
        match item with
        | StrValue v -> v
        | _ -> ""


    let ToRtl (numbers: string array) =
        let longest = numbers |> Array.map _.Trim().Length |> Array.max
        let transformed = numbers |> Array.map _.PadRight(longest, ' ')
        let results = Array.zeroCreate longest

        for i = longest downto 1 do
            results[i - 1] <-
                transformed
                |> Array.map (fun x -> if x[i - 1] = ' ' then -1L else int64 (x[i - 1] - '0'))
                |> Array.filter ((<>) -1L)
                |> Array.fold (fun acc curr -> acc * 10L + curr) 0L

        results

    interface IDay with
        member _.Day = 6

        member this.Part1() =
            Input.readLines (this :> IDay).Day
            |> Array.map (fun s ->
                s.Split(' ')
                |> Array.filter (fun x -> x.Trim() <> "")
                |> Array.map (fun y ->
                    match y with
                    | "+" -> Operator('+')
                    | "*" -> Operator('*')
                    | _ -> Value(int64 y)))
            |> Transpose
            |> Array.map (fun arr -> (arr[arr.Length - 1], arr[.. arr.Length - 2] |> Array.map ItemToInt))
            |> Array.map (fun (x, y) ->
                match x with
                | Operator('+') -> y.Sum()
                | Operator('*') -> y |> Array.fold (*) 1
                | _ -> 0)
            |> Array.sum
            |> string


        member this.Part2() =
            let content = Input.readLinesNoTrim (this :> IDay).Day

            let operatorsIndexes =
                content[content.Length - 1]
                |> Seq.mapi (fun i c -> i, c)
                |> Seq.filter (fun (_, c) -> c <> ' ')
                |> Seq.map fst
                |> Seq.toArray

            content
            |> Array.mapi (fun lineIdx line ->
                operatorsIndexes
                |> Array.mapi (fun i startIdx ->
                    let endIdx =
                        if i + 1 < operatorsIndexes.Length then
                            operatorsIndexes[i + 1] - 1
                        else
                            line.Length - 1

                    let chunk = line.Substring(startIdx, endIdx - startIdx + 1)

                    if lineIdx = content.Length - 1 then
                        if (chunk[0] = '+' || chunk[0] = '*') then
                            Operator chunk[0]
                        else
                            StrValue chunk
                    else
                        StrValue chunk))
            |> Transpose
            |> Array.map (fun arr -> (arr[arr.Length - 1], arr[.. arr.Length - 2] |> Array.map ItemToStr |> ToRtl))
            |> Array.map (fun (x, y) ->
                match x with
                | Operator('+') -> y.Sum()
                | Operator('*') -> y |> Array.fold (*) 1
                | _ -> 0)
            |> Array.sum
            |> string
