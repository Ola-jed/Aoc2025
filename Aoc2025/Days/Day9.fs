module Aoc2025.Day9

open Aoc2025.Days

type Point2D = { X: int64; Y: int64 }

type Rectangle =
    { TL: Point2D
      TR: Point2D
      BL: Point2D
      BR: Point2D }


type Day9() =
    let inferRectangle a b =
        if a.X = b.X then
            let leftmost = if a.Y < b.Y then a else b
            let rightmost = if a.Y < b.Y then b else a

            { TL = leftmost
              BL = leftmost
              TR = rightmost
              BR = rightmost }
        else if a.Y = b.Y then
            let topmost = if a.X < b.X then a else b
            let bottommost = if a.X < b.X then b else a

            { TL = topmost
              BL = bottommost
              TR = topmost
              BR = bottommost }
        else
            let topmost = if a.X < b.X then a else b
            let bottommost = if a.X < b.X then b else a

            if topmost.Y < bottommost.Y then
                { TL = topmost
                  BL = { X = bottommost.X; Y = topmost.Y }
                  TR = { X = topmost.X; Y = bottommost.Y }
                  BR = bottommost }
            else
                { TL = { X = topmost.X; Y = bottommost.Y }
                  BL = bottommost
                  TR = topmost
                  BR = { X = bottommost.X; Y = topmost.Y } }


    let area rectangle =
        if rectangle.TL = rectangle.BL then
            rectangle.TR.Y - rectangle.TL.Y
        else if rectangle.TL = rectangle.TR then
            rectangle.BL.Y - rectangle.TL.X
        else
            (rectangle.TR.Y - rectangle.TL.Y + 1L) * (rectangle.BL.X - rectangle.TL.X + 1L)

    interface IDay with
        member _.Day = 9

        member this.Part1() =
            let redTiles =
                Input.readLines (this :> IDay).Day
                |> Array.map (fun line ->
                    let parts = line.Split(',')

                    { X = int64 parts[1]
                      Y = int64 parts[0] })

            [ for i in 0 .. redTiles.Length - 2 do
                  for j in i + 1 .. redTiles.Length - 1 do
                      let a, b = redTiles[i], redTiles[j]
                      yield a, b ]
            |> List.map (fun (a, b) -> inferRectangle a b)
            |> List.map area
            |> List.max
            |> string

        member this.Part2() = ""
