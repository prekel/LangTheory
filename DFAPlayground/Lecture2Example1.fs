module Lecture2Example1

type State =
    | State_q0
    | State_q1
    | State_q2

let delta a q =
    match q with
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
        | [ x ] -> delta x q
        | x :: xs -> delta x q |> dfaCycle xs

    match dfaCycle (str |> List.ofSeq) State_q0 with
    | State_q2 -> true
    | _ -> false
