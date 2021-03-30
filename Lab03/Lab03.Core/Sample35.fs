module Lab03.Core.Sample35

type State =
    | Q0
    | Q1
    | Q2

type Input =
    | I0
    | I1

type Stack =
    | S0
    | S1
    | Z0

let delta q a X =
    match q, a, X with
    | Q0, Some I0, Z0 -> Set.singleton Q0, [ S0; Z0 ]
    | Q0, Some I1, Z0 -> Set.singleton Q0, [ S1; Z0 ]
    | Q0, Some I0, S0 -> Set.singleton Q0, [ S0; S0 ]
    | Q0, Some I0, S1 -> Set.singleton Q0, [ S0; S1 ]
    | Q0, Some I1, S0 -> Set.singleton Q0, [ S1; S0 ]
    | Q0, Some I1, S1 -> Set.singleton Q0, [ S1; S1 ]
    | Q0, None, Z0 -> Set.singleton Q1, [ Z0 ]
    | Q0, None, S0 -> Set.singleton Q1, [ S0 ]
    | Q0, None, S1 -> Set.singleton Q1, [ S1 ]
    | Q1, Some I0, S0 -> Set.singleton Q1, []
    | Q1, Some I1, S1 -> Set.singleton Q1, []
    | Q1, None, Z0 -> Set.singleton Q2, [ Z0 ]
    | _ -> Set.empty, []

let charToAlphabet =
    function
    | '0' -> Some I0
    | '1' -> Some I1
    | _ -> None

open Pda

let pda =
    { Pda.Transition = delta
      Initial = Q0
      Final = Set.singleton Q2
      StackInitial = Z0 }
