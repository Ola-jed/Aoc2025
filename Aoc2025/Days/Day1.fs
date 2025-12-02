module Aoc2025.Day1

open Aoc2025.Days

type Day1() =
    static member private PositiveModulo100 number = (number % 100 + 100) % 100

    interface IDay with
        member this.Day = 1

        member this.Part1() =
            let _, pwd =
                (this :> IDay).Day
                |> Input.readLines
                |> Array.map (fun s -> if s[0] = 'L' then - int(s[1..]) else int (s[1..]))
                |> Array.fold
                    (fun (dialPos, pwdAcc) step ->
                        let newDialPos = Day1.PositiveModulo100(dialPos + step)
                        let pwdAcc' = if newDialPos = 0 then pwdAcc + 1 else pwdAcc
                        (newDialPos, pwdAcc'))
                    (50, 0)

            string pwd

        member this.Part2() =
            let _, pwd =
                (this :> IDay).Day
                |> Input.readLines
                |> Array.collect (fun s ->
                    let n = int s.[1..]
                    let step = if s.[0] = 'L' then -1 else 1
                    Array.init n (fun _ -> step))
                |> Array.fold
                    (fun (dialPos, pwdAcc) step ->
                        let newDialPos = Day1.PositiveModulo100(dialPos + step)
                        let pwdAcc' = if newDialPos = 0 then pwdAcc + 1 else pwdAcc
                        (newDialPos, pwdAcc'))
                    (50, 0)

            string pwd
