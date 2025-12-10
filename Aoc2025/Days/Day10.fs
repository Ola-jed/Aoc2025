module Aoc2025.Day10

open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions
open Aoc2025.Days
open Google.OrTools.LinearSolver

type Machine =
    { LightDiagram: int
      Buttons: int array array
      JoltageRequirements: int array }

type Day10() =
    let setBit number position = number ||| (1 <<< position)

    let unsetBit number position = number &&& (~~~(1 <<< position))

    let toggleBit number position = number ^^^ (1 <<< position)

    let applyButton button number = button |> Array.fold toggleBit number

    let parseLine str =
        let lightDiagram =
            Regex.Match(str, @"\[(.*?)\]").Groups[1].Value.ToCharArray()
            |> Seq.indexed
            |> Seq.fold (fun acc (i, c) -> if c = '#' then setBit acc i else acc) 0

        let buttons =
            Regex.Matches(str, @"\((.*?)\)")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Groups[1].Value.Split(',') |> Array.map int)
            |> Seq.toArray

        let joltageRequirements =
            Regex.Match(str, @"\{(.*?)\}").Groups[1].Value.Split(",")
            |> Array.map int

        { LightDiagram = lightDiagram
          Buttons = buttons
          JoltageRequirements = joltageRequirements }

    let bfs (target: int) (buttons: int array array) =
        let visited = HashSet<int>()
        let queue = Queue<int * int>()

        // state, button presses
        queue.Enqueue((0, 0))
        visited.Add 0 |> ignore
        let mutable buttonPresses = 9999999

        while queue.Count > 0 do
            let state, presses = queue.Dequeue()

            if state = target then
                buttonPresses <- min buttonPresses presses

            for button in buttons do
                let temp = applyButton button state

                if visited.Contains(temp) = false then
                    visited.Add temp |> ignore
                    queue.Enqueue((temp, presses + 1))

        buttonPresses


    let modelIp machine =
        let solver = Solver.CreateSolver("SCIP")

        let variables =
            machine.Buttons
            |> Array.map (fun i -> solver.MakeIntVar(0.0, infinity, $"x_{i}"))

        for joltageIdx in 0 .. machine.JoltageRequirements.Length - 1 do
            let joltageRequirement = machine.JoltageRequirements[joltageIdx]
            let cons = solver.MakeConstraint(joltageRequirement, joltageRequirement)

            for index in 0 .. machine.Buttons.Length - 1 do
                let button = machine.Buttons[index]
                let variable = variables[index]
                let coefficient = if button.Contains(joltageIdx) then 1 else 0
                cons.SetCoefficient(variable, coefficient)


        let objective = solver.Objective()
        for variable in variables do
            objective.SetCoefficient(variable, 1)

        objective.SetMinimization()
        solver


    interface IDay with
        member _.Day = 10

        member this.Part1() =
            Input.readLines (this :> IDay).Day
            |> Array.map parseLine
            |> Array.map (fun x -> bfs x.LightDiagram x.Buttons)
            |> Array.sum
            |> string


        member this.Part2() =
            Input.readLines (this :> IDay).Day
            |> Array.map parseLine
            |> Array.map modelIp
            |> Array.map (fun x ->
                x.Solve() |> ignore
                x.Objective().Value())
            |> Array.sum
            |> string
