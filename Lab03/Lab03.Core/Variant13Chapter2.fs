module Lab03.Core.Variant13Chapter2

type State =
    | Q0
    | Q1
    | Q2

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
        | Some IA, SA -> Set.singleton Q0, [ SA; SA ]
        | Some IA, SB -> Set.singleton Q0, [ SA; SB ]
        | Some IA, SC -> Set.singleton Q0, []
        | Some IA, Z0 -> Set.singleton Q0, [ SA; Z0 ]
        | Some IB, SA -> Set.singleton Q0, [ SB; SA ]
        | Some IB, SB -> Set.singleton Q0, [ SB; SB ]
        | Some IB, SC -> Set.singleton Q0, []
        | Some IB, Z0 -> Set.singleton Q0, [ SB; Z0 ]
        | Some IC, SA -> Set.singleton Q0, []
        | Some IC, SB -> Set.singleton Q0, []
        | None, SA -> Set.singleton Q1, []
        | None, SB -> Set.singleton Q1, []
        | None, SC -> Set.singleton Q1, []
        | Some IC, Z0 -> Set.singleton Q0, [ SC; Z0 ]
        | Some IC, SC -> Set.singleton Q0, [ SC; SC ]
        | _ -> Set.empty, []
    | Q1 ->
        match a, X with
        | None, SA -> Set.singleton Q1, []
        | None, SB -> Set.singleton Q1, []
        | None, SC -> Set.singleton Q1, []
        | None, SZ -> Set.singleton Q2, []
        | _ -> Set.empty, []
    | Q2 -> Set.empty, []

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
      Final = Set.singleton Q2
      StackInitial = Z0 }
