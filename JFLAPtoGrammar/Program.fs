open FSharp.Data

type Jflap = XmlProvider<"LL_binary_v5.jff">

let rightToString (right: Jflap.Right) =
    match right.Number with
    | Some n -> string n
    | None -> right.String |> Option.defaultValue ""

let productionsToTupleSeq (p: Jflap.Production []) =
    seq {
        for i in p do
            yield i.Left, rightToString i.Right
    }

let productionSeqToMap s =
    s
    |> Seq.groupBy fst
    |> Seq.map (fun (a, s) -> a, s |> Seq.map snd |> Seq.toArray)

let productionMapToStrings m =
    m
    |> Seq.map (fun (k, t) -> k, t |> Array.map string |> String.concat " | ")
    |> Seq.map (fun (k, t) -> $"%s{k} -> %s{t}")

[<EntryPoint>]
let main argv =
    let a =
        match argv |> Array.toList with
        | [ a ] -> Jflap.Load(a)
        | _ -> Jflap.GetSample()

    let m =
        a.Productions
        |> productionsToTupleSeq
        |> productionSeqToMap
        |> productionMapToStrings

    for i in m do
        printfn $"%s{i}"

    0
