open System

open Lab03.Core

type PdaVar =
    | Sample35
    | Variant13Chapter1
    | Variant13Chapter2

let pdaVarToPda pdaVar str =
    let toBoolOption pda charToAlphabet =
        str
        |> Seq.map charToAlphabet
        |> Seq.toList
        |> Pda.liftListOption
        |> Option.map (Pda.pdaCheck pda)

    match pdaVar with
    | Sample35 -> toBoolOption Sample35.pda Sample35.charToAlphabet
    | Variant13Chapter1 -> toBoolOption Variant13Chapter1.pda Variant13Chapter1.charToAlphabet
    | Variant13Chapter2 -> toBoolOption Variant13Chapter2.pda Variant13Chapter2.charToAlphabet

let input () = Console.ReadLine()

let inputTask () =
    match Console.ReadLine() with
    | "1"
    | "Sample35" -> Ok Sample35
    | "2"
    | "Variant13Chapter1" -> Ok Variant13Chapter1
    | "3"
    | "Variant13Chapter2" -> Ok Variant13Chapter2
    | _ -> Error "Неверный номер автомата"

let boolToResult =
    function
    | false -> "Отвергнуто"
    | true -> "Принято"

[<EntryPoint>]
let main argv =
    //Console.OutputEncoding <- Encoding.Unicode
    //Console.InputEncoding <- Encoding.Unicode

    printfn
        "%s\n%s\n%s"
        "1 -> Sample35 : Чётный палиндром, алфавит {0, 1}"
        "2 -> Variant13Chapter1 : w_1cw_2 : w_1 != w_2^R, и w_1, w_2 принадлежат {a, b}*, алфавит {a, b, c}"
        "3 -> Variant13Chapter2 : w принадлежит {a, b, c}* : n_a(w) + n_b(w) != n_c(w), алфавит {a, b, c}"

    printfn "%s" "Введите номер автомата: "

    match inputTask () with
    | Ok pdaVar ->
        printf "%s" "Введите цепочку: "
        let str = input ()
        let t = pdaVarToPda pdaVar str

        match t with
        | Some y -> printfn $"%s{boolToResult y}"
        | None -> printfn "Неверный символ"
    | Error s -> printfn $"%s{s}"

    0
