open System

open Lab03.Core

[<EntryPoint>]
let main argv =

    let str = Console.ReadLine()

    let s =
        str
        |> Seq.toList
        |> List.map (Variant13.charToAlphabet)
        |> Pda.liftListOption

    match s with
    | Some s ->
        let a = Pda.pdaSolve Variant13.pda s
        printfn "%A" a
    | None -> ()

    0
