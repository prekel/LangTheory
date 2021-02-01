module Lecture1Example2

type State =
    | Q0
    | Q1
    | Q2
    | Q3

type Alphabet =
    | A0 // '0'
    | A1 // '1'

let delta q a =
    match q with
    | Q0 ->
        match a with
        | A0 -> Q2
        | A1 -> Q1
    | Q1 ->
        match a with
        | A0 -> Q3
        | A1 -> Q0
    | Q2 ->
        match a with
        | A0 -> Q0
        | A1 -> Q3
    | Q3 ->
        match a with
        | A0 -> Q1
        | A1 -> Q2

let charToAlphabet char =
    match char with
    | '0' -> A0
    | '1' -> A1
    | _ -> failwith "never"

let verifyA str =
    CustomDfa.verifyCustomDfa Q0 Q0 delta str

let verify str = str |> Seq.map charToAlphabet |> verifyA

open CustomDfa

let dfa =
    { Dfa0.Initial = Q0
      Final = Q0
      Delta = delta }
