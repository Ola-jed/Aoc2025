namespace Aoc2025

open System.IO
open System.Reflection

module Input =
    let private assembly = Assembly.GetExecutingAssembly()
    let private baseName = "Aoc2025.Inputs"

    let getInput day =
        let name = $"{baseName}.day{day}.txt"
        use stream = assembly.GetManifestResourceStream(name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let readLines day =
        getInput day
        |> _.Split('\n')
        |> Array.map _.Trim()
        |> Array.filter (fun line -> line <> "")

    let readLinesNoTrim day =
        getInput day
        |> _.Split('\n')
        |> Array.map _.Replace("\r", "")
        |> Array.map _.Replace("\n", "")
