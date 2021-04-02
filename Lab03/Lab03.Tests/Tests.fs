module Lab03.Tests

open NUnit.Framework
open Swensen.Unquote

open Lab03.Core

let private brute inputAlphabet length =
    []
    :: ([ 1 .. length ]
        |> List.collect
            (fun n ->
                (inputAlphabet |> List.map List.singleton, [ 1 .. n - 1 ])
                ||> List.fold (fun st _ -> List.collect (fun l -> inputAlphabet |> List.map (fun g -> g :: l)) st)
                |> List.sort))

[<Test>]
let ``Тестирование функции, которая перебирает строки`` () =
    let values = [ 1; 2; 3 ]

    let v0 : int list list = [ [] ]
    let v1 = [ [ 1 ]; [ 2 ]; [ 3 ] ]

    let v2 =
        [ [ 1; 1 ]
          [ 1; 2 ]
          [ 1; 3 ]
          [ 2; 1 ]
          [ 2; 2 ]
          [ 2; 3 ]
          [ 3; 1 ]
          [ 3; 2 ]
          [ 3; 3 ] ]

    test <@ (v0 @ v1 @ v2 |> List.sort) = (brute values 2 |> List.sort) @>

module Sample35Tests =
    open Lab03.Core.Sample35

    [<Test>]
    let ``Преобразование строки в список символов алфавита для примера 35`` () =
        let str = "001100"

        let expected = Some [ I0; I0; I1; I1; I0; I0 ]

        test
            <@ expected = (str
                           |> Seq.map charToAlphabet
                           |> Seq.toList
                           |> Pda.liftListOption) @>

    [<Test>]
    let ``Все возможные строки длиной от 0 до 10 для примера 35`` () =
        let values = [ I0; I1 ]

        let y = brute values 10

        let checker (l: Input list) =
            match (l |> List.length) % 2, l |> List.rev with
            | 0, r when r = l -> true
            | _ -> false

        y
        |> List.iter (fun l -> test <@ checker l = (Pda.pdaSolve pda l |> Pda.pdaCheck1 pda) @>)

module Variant13Chapter1Tests =
    open Variant13Chapter1

    [<Test>]
    let ``Преобразование строки в список символов алфавита для Варианта 13 (Часть 1)`` () =
        let str = "abcba"

        let expected = Some [ IA; IB; IC; IB; IA ]

        test
            <@ expected = (str
                           |> Seq.map charToAlphabet
                           |> Seq.toList
                           |> Pda.liftListOption) @>

    [<Test>]
    let ``Все возможные строки длиной от 0 до 9 для Варианта 13 (Часть 1)`` () =
        let values = [ IA; IB; IC ]

        let y = brute values 9

        let checker (l: Input list) =
            match l |> List.filter ((=) IC) |> List.length with
            | 1 ->
                let beforeC = l |> List.takeWhile ((<>) IC)

                let afterC =
                    l
                    |> List.skip (List.length beforeC + 1)
                    |> List.rev

                beforeC <> afterC
            | _ -> false

        y
        |> List.iter (fun l -> test <@ checker l = (Pda.pdaSolve pda l |> Pda.pdaCheck1 pda) @>)

module Variant13Chapter2Tests =
    open Variant13Chapter2

    [<Test>]
    let ``Преобразование строки в список символов алфавита для Варианта 13 (Часть 2)`` () =
        let str = "abbca"

        let expected = Some [ IA; IB; IB; IC; IA ]

        test
            <@ expected = (str
                           |> Seq.map charToAlphabet
                           |> Seq.toList
                           |> Pda.liftListOption) @>

    [<Test>]
    let ``Преобразование неверной строки в список символов алфавита для Варианта 13 (Часть 2)`` () =
        let str = "aboba"

        let expected : Input list option = None

        test
            <@ expected = (str
                           |> Seq.map charToAlphabet
                           |> Seq.toList
                           |> Pda.liftListOption) @>

    [<Test>]
    let ``Все возможные строки длиной от 0 до 8 для Варианта 13 (Часть 2)`` () =
        let values = [ IA; IB; IC ]

        let y = brute values 8

        let checker (l: Input list) =
            let count a = List.filter ((=) a) l |> List.length

            count IA + count IB <> count IC

        y
        |> List.iter (fun l -> test <@ checker l = (Pda.pdaSolve pda l |> Pda.pdaCheck1 pda) @>)
