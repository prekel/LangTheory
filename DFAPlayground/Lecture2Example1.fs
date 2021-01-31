module Lecture2Example1

type State =
    | State_q0
    | State_q1
    | State_q2

let delta q a =
    match   q with
    | State_q0 ->
        match a with
        | '0' -> State_q1
        | '1' -> State_q0
    | State_q1 ->
        match a with
        | '0' -> State_q1
        | '1' -> State_q2
    | State_q2 -> State_q2

let verify str =
    let rec dfaCycle s q =
        match s with
        | [] -> State_q0
        | [ x ] -> delta q x
        | x :: xs -> delta q x |> dfaCycle xs

    match dfaCycle (str |> List.ofSeq) State_q0 with
    | State_q2 -> true
    | _ -> false

let verifyViaFold str =
    (State_q0, str |> List.ofSeq)
    ||> List.fold delta
    |> (=) State_q2

let verify1 str =
    CustomDfa.verifyCustomDfa State_q0 State_q2 delta str
