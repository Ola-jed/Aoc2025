module Aoc2025.Day10

open System.Text.RegularExpressions
open Aoc2025.Days

type Machine =
    { LightDiagram: int
      Buttons: int array array
      JoltageRequirements: int array }

type Day10() =
    let setBit number position = number ||| (1 <<< position)

    let unsetBit number position = number &&& (~~~(1 <<< position))

    let parseLine str =
        let lightDiagram =
            Regex.Match(str, @"\[(.*?)\]").Groups.[1].Value.ToCharArray()
            |> Seq.indexed
            |> Seq.fold (fun acc (i, c) -> if c = '#' then setBit acc i else acc) 0

        let buttons =
            Regex.Matches(str, @"\((.*?)\)")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Groups.[1].Value.Split(',') |> Array.map int)
            |> Seq.toArray

        let joltageRequirements =
            Regex.Match(str, @"\{(.*?)\}").Groups.[1].Value.Split(",")
            |> Array.map (fun x -> (int (x) - int ('0')))

        { LightDiagram = lightDiagram
          Buttons = buttons
          JoltageRequirements = joltageRequirements }

    interface IDay with
        member _.Day = 10

        member this.Part1() = ""


        member this.Part2() = ""
