module Else

type Q =
    | Empty
    | E
    | El
    | Els
    | Else
    | Undefined of Q

let delta q a =
    match q with
    | Empty ->
        match a with
        | 'e' -> E
        | _ -> Undefined q
    | E ->
        match a with
        | 'l' -> El
        | _ -> Undefined q
    | El ->
        match a with
        | 's' -> Els
        | _ -> Undefined q
    | Els ->
        match a with
        | 'e' -> Else
        | _ -> Undefined q
    | Else -> Undefined q
    | Undefined a -> Undefined a


open CustomDfa

let Dfa =
    { Dfa4.Delta = delta
      Initial = Empty
      IsFinal = (=) Else }
