namespace Aoc2025

type IDay =
    abstract member Day : int
    abstract member Part1 : unit -> string
    abstract member Part2 : unit -> string