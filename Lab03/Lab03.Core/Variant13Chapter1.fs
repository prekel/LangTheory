module Lab03.Core.Variant13Chapter1

type State =
    | Q0
    | Q1
    | Q2
    | Q3
    | Q4

type Input =
    | IA
    | IB
    | IC

type Stack =
    | SA
    | SB
    | SC
    | Z0

let delta q a X =
    match q with
    | Q0 ->
        match a, X with
        | Some IB, SA -> Set.singleton Q0, [ SB; SA ]
        | Some IB, Z0 -> Set.singleton Q0, [ SB; Z0 ]
        | Some IB, SB -> Set.singleton Q0, [ SB; SB ]
        | Some IA, SA -> Set.singleton Q0, [ SA; SA ]
        | Some IA, SB -> Set.singleton Q0, [ SA; SB ]
        | Some IA, Z0 -> Set.singleton Q0, [ SA; Z0 ]
        | Some IC, SC -> Set.singleton Q1, [ SC ]
        | Some IC, SB -> Set.singleton Q1, [ SB ]
        | Some IC, SA -> Set.singleton Q1, [ SA ]
        | Some IC, Z0 -> Set.singleton Q2, [ Z0 ]
        | _ -> Set.empty, []
    | Q1 ->
        match a, X with
        | None, SA -> Set.singleton Q2, [ SA ]
        | None, Z0 -> Set.singleton Q2, [ Z0 ]
        | None, SB -> Set.singleton Q2, [ SB ]
        | _ -> Set.empty, []
    | Q2 ->
        match a, X with
        | Some IA, SA -> Set.singleton Q2, []
        | Some IB, SB -> Set.singleton Q2, []
        | Some IA, SB -> Set.singleton Q3, []
        | Some IB, SA -> Set.singleton Q3, []
        | Some IA, Z0 -> Set.singleton Q3, [ Z0 ]
        | Some IB, Z0 -> Set.singleton Q3, [ Z0 ]
        | None, SA -> Set.singleton Q4, []
        | None, SB -> Set.singleton Q4, []
        | _ -> Set.empty, []
    | Q3 ->
        match a, X with
        | Some IA, _ -> Set.singleton Q3, [ X ]
        | Some IB, _ -> Set.singleton Q3, [ X ]
        | _ -> Set.empty, []
    | Q4 -> Set.empty, []

let charToAlphabet =
    function
    | 'a' -> Some IA
    | 'b' -> Some IB
    | 'c' -> Some IC
    | _ -> None

open Pda

let pda =
    { Pda.Transition = delta
      Initial = Q0
      Final = Set.ofList [ Q1; Q3; Q4 ]
      StackInitial = Z0 }
