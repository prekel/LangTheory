module Lab03.Core.Variant13

type State =
    | Q0
    | Q1
    | Q2
    | Q3

type Input =
    | IA
    | IB
    | IC

type Stack =
    | SA
    | SB
    | SC
    | Z0

let delta (q: State) (a: Input option) (X: Stack) =
    match q, a, X with
    | _ -> Set.empty, []

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
