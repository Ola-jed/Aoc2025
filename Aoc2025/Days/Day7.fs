module Aoc2025.Day7

open Aoc2025.Days


type Day7() =
    interface IDay with
        member _.Day = 7

        member this.Part1() =
            let content = Input.readLines (this :> IDay).Day |> Array.map _.ToCharArray()
            let startingColumn = content[0] |> Array.findIndex ((=) 'S')
            let height = content.Length
            let width = content[0].Length
            let mutable splitCount = 0
            content[1][startingColumn] <- '|'

            for row in 1 .. (height - 2) do
                let nextRow = row + 1

                for column in 0 .. (width - 1) do
                    if content[row][column] = '|' then
                        match content[nextRow][column] with
                        | '^' ->
                            splitCount <- splitCount + 1

                            if column > 0 then
                                content[nextRow][column - 1] <- '|'

                            if column < width - 1 then
                                content[nextRow][column + 1] <- '|'

                        | _ -> content[nextRow][column] <- '|'

            string splitCount


        member this.Part2() =
            let content = Input.readLines (this :> IDay).Day |> Array.map _.ToCharArray()
            let startingColumn = content[0] |> Array.findIndex ((=) 'S')
            let height = content.Length
            let width = content[0].Length
            let beams = Array.init width (fun _ -> 0L)

            content[1][startingColumn] <- '|'
            beams[startingColumn] <- 1

            for row in 1 .. (height - 2) do
                let nextRow = row + 1

                for column in 0 .. (width - 1) do
                    if content[row][column] = '|' then
                        match content[nextRow][column] with
                        | '^' ->
                            if column > 0 then
                                beams[column - 1] <- beams[column - 1] + beams[column]
                                content[nextRow][column - 1] <- '|'

                            if column < width - 1 then
                                beams[column + 1] <- beams[column + 1] + beams[column]
                                content[nextRow][column + 1] <- '|'

                            beams[column] <- 0
                        | _ -> content[nextRow][column] <- '|'

            beams |> Array.sum |> string
