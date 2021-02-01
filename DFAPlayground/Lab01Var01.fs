module Lab01Var01

type Q =
    | Q0
    | Q1
    | Q2
    | Q3

type A =
    | A
    | B

let delta q a =
    match q with
    | Q0 ->
        match a with
        | A -> Q1
        | B -> Q0
    | Q1 ->
        match a with
        | A -> Q2
        | B -> Q1
    | Q2 ->
        match a with
        | A -> Q3
        | B -> Q2
    | Q3 -> Q3

let deltaViaFunction =
    function
    | Q0 ->
        function
        | A -> Q1
        | B -> Q0
    | Q1 ->
        function
        | A -> Q2
        | B -> Q1
    | Q2 ->
        function
        | A -> Q3
        | B -> Q2
    | Q3 -> (fun _ -> Q3)

let isFinal q =
    match q with
    | Q3 -> true
    | _ -> false

let deltaViaTable q a =
    Map.ofList [ ((Q0, A), Q1)
                 ((Q0, B), Q0)
                 ((Q1, A), Q2)
                 ((Q1, B), Q1)
                 ((Q2, A), Q3)
                 ((Q2, B), Q2)
                 ((Q3, A), Q3)
                 ((Q3, B), Q3) ]
    |> Map.find (q, a)


open CustomDfa

let Dfa =
    { Delta = deltaViaTable
      Initial = Q0
      Final = Set.singleton Q3 }

let Dfa4 =
    { Dfa4.Delta = deltaViaTable
      Initial = Q0
      IsFinal = (Set.singleton Q3).Contains }

let Dfa44 =
    { Dfa4.Delta = deltaViaFunction
      Initial = Q0
      IsFinal = isFinal }
