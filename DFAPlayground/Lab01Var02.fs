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

open CustomDfa

let Dfa =
    { Initial = Q0
      Final = Q3
      Delta = delta }
