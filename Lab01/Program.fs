open System
open System.Text

type Task =
    | Dfa
    | Nfa

let boolToResult =
    function
    | false -> "Reject"
    | true -> "Accept"

let printStateResult state result =
    printfn "Состояние: %A" state
    printfn "Результат: %s" (boolToResult result)

let proceedDfa str =
    let dfa = Dfa.Dfa
    let state = Fa.stateDfa dfa str
    let isFinal = Fa.verifyDfa dfa str
    printStateResult state isFinal

let proceedNfa str =
    let nfa = Nfa.Nfa
    let states = Fa.stateNfa nfa str
    let isFinal = Fa.verifyNfa nfa str
    printStateResult states isFinal

let input () = Console.ReadLine()

let inputTask () =
    match Console.ReadLine() with
    | "1"
    | "a"
    | "а" -> Dfa
    | "2"
    | "b"
    | "б" -> Nfa
    | _ -> failwith "Неверный выбор"

let stringToSeq (str: string) charToAlphabet = str |> Seq.map charToAlphabet

[<EntryPoint>]
let main _ =
    //Console.OutputEncoding <- Encoding.Unicode
    //Console.InputEncoding <- Encoding.Unicode
    
    try
        printf "%s" "Введите номер задания (1/2, a/b, а/б): "
        let task = inputTask ()
        printf "%s" "Введите цепочку: "
        let str = input ()

        match task with
        | Dfa ->
            (str, Dfa.charToAlphabet)
            ||> stringToSeq
            |> proceedDfa
        | Nfa ->
            (str, Nfa.charToAlphabet)
            ||> stringToSeq
            |> proceedNfa
    with ex -> printfn "Ошибка: %s" ex.Message

    0
