module Aoc2025.Day11

open System.Collections.Generic
open System.Linq
open Aoc2025.Days

type Device = { Id: string; Neighbors: string array }

type DeviceNode =
    { Id: string
      Children: DeviceNode list }

type Day11() =
    let parseLine (line: string) =
        let parts = line.Split(":")
        let connected = parts[1].Split(" ")

        { Id = parts[0].Trim()
          Neighbors = connected |> Array.map _.Trim() }

    let getPaths1 root =
        let stack = Stack<DeviceNode>()
        let mutable outs = 0
        stack.Push(root)

        while stack.Count > 0 do
            let top = stack.Pop()

            if top.Id = "out" then
                outs <- outs + 1

            for child in top.Children do
                stack.Push(child)

        outs
        
    let getPaths2 root =
        let trajectories = HashSet<int>()
        let stack = Stack<DeviceNode>()
        let mutable prolematicPaths = 0
        stack.Push(root)

        while stack.Count > 0 do
            let top = stack.Peek()
            if top.Id = "out" then
                let foundFftAndDac =
                    stack
                    |> Seq.fold (fun (foundFft, foundDac) x ->
                        let foundFft' = foundFft || (x.Id = "fft")
                        let foundDac' = foundDac || (x.Id = "dac")
                        (foundFft', foundDac')
                    ) (false, false)
                    |> fun (fFft, fDac) -> fFft && fDac

                if foundFftAndDac then
                    prolematicPaths <- prolematicPaths + 1

            let mutable visitedAllChildren = true
            let mutable index = 0
            while index < top.Children.Length && visitedAllChildren do
                let child = top.Children[index]
                
                stack.Push(child)
                let trajectory = String.concat " " (stack |> Seq.map _.Id) |> hash
                stack.Pop() |> ignore
                
                if trajectories.Contains(trajectory) = false then
                    stack.Push(child)
                    trajectories.Add(trajectory) |> ignore
                    visitedAllChildren <- false
                
                index <- index + 1
           
            if visitedAllChildren then
                stack.Pop() |> ignore

        prolematicPaths

    interface IDay with
        member _.Day = 11

        member this.Part1() =
            let devices = Input.readLines (this :> IDay).Day |> Array.map parseLine
            let allDevices = [| yield! devices; yield { Id = "out"; Neighbors = [||] } |]
            let deviceMap = allDevices |> Array.map (fun d -> d.Id, d) |> Map.ofArray

            let rec buildTree (id: string) : DeviceNode =
                let device = deviceMap[id]
                let children =
                    device.Neighbors
                    |> Array.filter deviceMap.ContainsKey
                    |> Array.map buildTree
                    |> Array.toList

                { Id = id; Children = children }

            devices
            |> Array.find (fun x -> x.Id = "you")
            |> _.Id
            |> buildTree
            |> getPaths1
            |> string


        member this.Part2() = // Inefficient approach
            let devices = Input.readLines (this :> IDay).Day |> Array.map parseLine
            let allDevices = [| yield! devices; yield { Id = "out"; Neighbors = [||] } |]
            let deviceMap = allDevices |> Array.map (fun d -> d.Id, d) |> Map.ofArray

            let rec buildTree (id: string) : DeviceNode =
                let device = deviceMap[id]
                let children =
                    device.Neighbors
                    |> Array.filter deviceMap.ContainsKey
                    |> Array.map buildTree
                    |> Array.toList

                { Id = id; Children = children }

            devices
            |> Array.find (fun x -> x.Id = "svr")
            |> _.Id
            |> buildTree
            |> getPaths2
            |> string
