module Aoc2025.Day12

open Aoc2025.Days

type Shape =
    { Id: int
      Disposition: bool array array }

type Region =
    { Width: int
      Height: int
      ShapesQuantities: int array }

type Day12() =
    let lineIsShapeStart (line: string) = line.Split(":").[1].Trim() = ""

    let lineIsRegion (line: string) = line.Contains("x")

    let parseShape (lines: string array) =
        let id = int (lines[0][0]) - int '0'

        let shape =
            lines |> Array.map (fun x -> x |> Seq.toArray |> Array.map (fun y -> y = '#'))

        { Id = id; Disposition = shape }


    let parseRegion (line: string) =
        let mainParts = line.Split(":") |> Array.map _.Trim()
        let size = mainParts[0].Split("x")
        let shapesQuantities = mainParts[1].Split(" ")

        { Width = int (size[0])
          Height = int (size[1])
          ShapesQuantities = shapesQuantities |> Array.map int }

    let canFit (region: Region) (shapes: Shape list) =
        let mutable neededArea = 0

        for i in 0 .. region.ShapesQuantities.Length - 1 do
            let quantity = region.ShapesQuantities[i]
            let shape = shapes[i]

            let shapeArea =
                shape.Disposition
                |> Array.sumBy (fun row -> row |> Array.sumBy (fun cell -> if cell then 1 else 0))

            neededArea <- neededArea + (quantity * shapeArea)

        (region.Height * region.Width) > neededArea


    interface IDay with
        member _.Day = 12

        member this.Part1() =
            let mutable shapes = []
            let mutable regions = []
            let mutable index = 0
            let lines = Input.readLines (this :> IDay).Day

            while index < lines.Length do
                let line = lines[index]

                if lineIsShapeStart line then
                    let shape = parseShape lines[index + 1 .. index + 3]
                    shapes <- shapes @ [ shape ]
                    index <- index + 4
                else if lineIsRegion line then
                    let region = parseRegion line
                    regions <- regions @ [ region ]
                    index <- index + 1
                else
                    index <- index + 1

            regions |> List.filter (fun x -> canFit x shapes) |> List.length |> string


        member this.Part2() = "Finish all the other puzzles"
