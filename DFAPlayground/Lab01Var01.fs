module Lab01Var02

type Q =
    | Q0
    | Q1
    | Q2
    | Q3
    | Q5

type A =
    | A1
    | A2
    | A3

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
    | Q3 ->
        match a with
        | A -> Q3
        | B -> Q2
    | Q3 -> Q3

open CustomDfa

let Dfa =
    { Initial = Q0
      Final = Q3
      Delta = delta }
