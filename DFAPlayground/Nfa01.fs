module Nfa01


type State =
    | Q0
    | Q1
    | Q2

type Alphabet =
    | A0
    | A1

let delta q a =
    match q with
    | Q0 ->
        match a with
        | A0 -> Set.ofList [ Q0; Q1 ]
        | A1 -> Set.singleton Q0
    | Q1 ->
        match a with
        | A0 -> Set.empty
        | A1 -> Set.singleton Q2
    | Q2 -> Set.empty

open CustomDfa

let Nfa =
    { Transition = delta
      Initial = Q0
      Final = Set.singleton Q2 }
