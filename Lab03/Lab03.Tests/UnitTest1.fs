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

[<Test>]
let ``qwe`` () =
    let values =
        [ None
          Some Sample35.I0
          Some Sample35.I1 ]

    let v2 =
        values
        |> List.collect (fun l -> values |> List.map (fun g -> [ l; g ]))
        |> List.collect (fun l -> values |> List.map (fun g -> l @ [ g ]))
        |> List.collect (fun l -> values |> List.map (fun g -> l @ [ g ]))
        |> List.collect (fun l -> values |> List.map (fun g -> l @ [ g ]))
        |> List.map (fun l -> l |> List.skipWhile Option.isNone)

    let values = [ Sample35.I0; Sample35.I1 ]


    let v4 =
        ([ values ], values)
        ||> List.fold (fun st t -> (st |> List.head |> List.tail) :: st)

    let v3 =
        [ 1 .. 4 ]
        |> List.collect
            (fun n ->
                ([], [ 1 .. n ])
                ||> List.fold
                        (fun st t ->
                            values
                            |> List.collect (fun l -> values |> List.map (fun g -> l :: [ g ]))))

    let v1 = values |> List.map (fun g -> [ g ])

    let v2 =
        values
        |> List.collect (fun l -> values |> List.map (fun g -> [ l; g ]))

    let v3 =
        values
        |> List.map (fun g -> [ g ])
        |> List.collect (fun l -> values |> List.map (fun g -> g :: l))
        |> List.collect (fun l -> values |> List.map (fun g -> g :: l))
        |> List.sort

    let v1 = values |> List.map (fun g -> [ g ])

    let f =
        List.collect (fun l -> values |> List.map (fun g -> g :: l))

    let vx n =
        (v1, [ 1 .. n - 1 ])
        ||> List.fold (fun st t -> f st)
        |> List.sort

    let v1 = vx 1
    let v2 = vx 2
    let v3 = vx 3
    let v123 = v1 @ v2 @ v3

    let f values =
        List.collect (fun l -> values |> List.map (fun g -> g :: l))

    let vxx values n =
        [ 1 .. n ]
        |> List.collect
            (fun n ->
                (values |> List.map List.singleton, [ 1 .. n - 1 ])
                ||> List.fold (fun st _ -> List.collect (fun l -> values |> List.map (fun g -> g :: l)) st)
                |> List.sort)

    let y = vxx values 3
    test <@ [] = y @>
