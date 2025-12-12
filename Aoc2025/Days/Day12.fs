module Aoc2025.Day12

open Aoc2025.Days

type Shape = { Id: int; Shape: bool array array }

type Region =
    { Width: int
      Height: int
      ShapesQuantities: int array }

type Day12() =
    let lineIsShapeStart (line: string) = line.Split(":").[1].Trim() = ""

    let lineIsRegion (line: string) = line.Contains("x")

    let parseShape (lines: string array) = // TODO : 4 lines
        let id = int(lines[0][0]) - int '0'
        ""
        
        
        
    let parseRegion (line: string) =
        let mainParts = line.Split(":") |> Array.map _.Trim()
        let size = mainParts[0].Split("x")
        let shapesQuantities = mainParts[1].Split(" ")
        {
            Width = int(size[0])
            Height = int(size[1])
            ShapesQuantities = shapesQuantities |> Array.map int
        }
        

    interface IDay with
        member _.Day = 12

        member this.Part1() = ""


        member this.Part2() = ""
