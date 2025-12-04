module Aoc2025.Day4

open Aoc2025.Days

type Day4() =
    static member private CanBeForklift(pos: int * int, grid: char array array) =
        let inline inBounds (x, y) =
            0 <= x && x < grid.Length && 0 <= y && y < grid[0].Length

        let x, y = pos

        let adjacentRolls =
            [| -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 |]
            |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
            |> Array.filter inBounds
            |> Array.map (fun (nx, ny) -> grid[nx][ny])
            |> Array.filter ((=) '@')
            |> Array.length

        adjacentRolls < 4


    interface IDay with
        member _.Day = 4

        member this.Part1() =
            let grid = Input.readLines (this :> IDay).Day |> Array.map _.ToCharArray()

            grid
            |> Array.mapi (fun x row -> row |> Array.mapi (fun y c -> (x, y, c)))
            |> Array.concat
            |> Array.filter (fun (_, _, z) -> z = '@')
            |> Array.filter (fun (x, y, _) -> Day4.CanBeForklift((x, y), grid))
            |> Array.length
            |> string


        member this.Part2() =
            let grid = Input.readLines (this :> IDay).Day |> Array.map _.ToCharArray()
            let mutable count = 0
            let mutable shouldRemove = true

            while shouldRemove do
                let indexesToRemove =
                    grid
                    |> Array.mapi (fun x row -> row |> Array.mapi (fun y c -> (x, y, c)))
                    |> Array.concat
                    |> Array.filter (fun (_, _, z) -> z = '@')
                    |> Array.filter (fun (x, y, _) -> Day4.CanBeForklift((x, y), grid))

                shouldRemove <- indexesToRemove.Length > 0
                count <- count + indexesToRemove.Length

                for x, y, _ in indexesToRemove do
                    grid[x][y] <- '.'


            string count
