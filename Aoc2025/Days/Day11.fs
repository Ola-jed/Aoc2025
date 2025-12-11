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

    let getPaths root =
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
            |> Array.filter (fun x -> x.Id = "you")
            |> Array.map (fun d -> buildTree d.Id)
            |> Array.map getPaths
            |> Array.sum
            |> string


        member _.Part2() = ""
