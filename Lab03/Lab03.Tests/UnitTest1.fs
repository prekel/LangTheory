module Lab03.Tests

open NUnit.Framework
open Swensen.Unquote

open Lab03.Core

[<SetUp>]
let Setup () = ()

[<Test>]
let ``demo Unquote NUnit support`` () =
    test <@ ([ 3; 2; 1; 0 ] |> List.map ((+) 1)) = [ 1 + 3 .. 1 + 0 ] @>

[<Test>]
let ``test1`` () =
    let str = "001100"

    let lst =
        str
        |> Seq.map Sample35.charToAlphabet
        |> Seq.toList
        |> Pda.liftListOption

    test
        <@ lst = (str
                  |> Seq.map Sample35.charToAlphabet
                  |> Seq.toList
                  |> Pda.liftListOption) @>

    test
        <@ true = (lst
                   |> Option.get
                   |> Pda.pdaSolve Sample35.pda
                   |> Pda.pdaCheck1 Sample35.pda) @>


let vxx inputAlphabet length =
    [ 1 .. length ]
    |> List.collect
        (fun n ->
            (inputAlphabet |> List.map List.singleton, [ 1 .. n - 1 ])
            ||> List.fold (fun st _ -> List.collect (fun l -> inputAlphabet |> List.map (fun g -> g :: l)) st)
            |> List.sort)

[<Test>]
let ``Все возможные строки длиной от 1 до 10 для примера 35`` () =
    let values = [ Sample35.I0; Sample35.I1 ]

    let y = vxx values 10

    let checker (l: Sample35.Input list) =
        match (l |> List.length) % 2, l |> List.rev with
        | 0, r when r = l -> true
        | _ -> false

    y
    |> List.iter
        (fun l ->
            test
                <@ checker l = (Pda.pdaSolve Sample35.pda l
                                |> Pda.pdaCheck1 Sample35.pda) @>)


[<Test>]
let ``Все возможные строки длиной от 1 до 10 для Варианта 1 (Часть 1)`` () =
    let values =
        [ Variant13Chapter1.IA
          Variant13Chapter1.IB
          Variant13Chapter1.IC ]

    let y = vxx values 10

    let checker (l: Variant13Chapter1.Input list) =
        match l
              |> List.filter ((=) Variant13Chapter1.IC)
              |> List.length with
        | 1 ->
            let beforeC =
                l |> List.takeWhile ((<>) Variant13Chapter1.IC)

            let afterC = l |> List.skip (List.length beforeC + 1) |> List.rev
            beforeC <> afterC
        | _ -> false

    y
    |> List.iter
        (fun l ->
            test
                <@ checker l = (Pda.pdaSolve Variant13Chapter1.pda l
                                |> Pda.pdaCheck1 Variant13Chapter1.pda) @>)
