module Lab01Var01b

type Q =
    | Initial
    | Only1
    | Only2
    | Only3
    | Not3
    | Not2
    | Not1
    | Not3Repeat
    | Not2Repeat
    | Not1Repeat
    | NewSymbol
    | AfterAllSymbols

type A =
    | A1
    | A2
    | A3

let delta q a =
    match q with
    | Initial ->
        match a with
        | A1 -> Only1
        | A2 -> Only2
        | A3 -> Only3
    | Only1 ->
        match a with
        | A1 -> Only1
        | A2 -> Not3
        | A3 -> Not2
    | Only2 ->
        match a with
        | A1 -> Not3
        | A2 -> Only2
        | A3 -> Not1
    | Only3 ->
        match a with
        | A1 -> Not2
        | A2 -> Not1
        | A3 -> Only3
    | Not3 ->
        match a with
        | A1 -> Not3Repeat
        | A2 -> Not3Repeat
        | A3 -> NewSymbol
    | Not2 ->
        match a with
        | A1 -> Not2Repeat
        | A2 -> NewSymbol
        | A3 -> Not2Repeat
    | Not1 ->
        match a with
        | A1 -> Not1Repeat
        | A2 -> NewSymbol
        | A3 -> Not1Repeat
    | Not3Repeat ->
        match a with
        | A1 -> Not3Repeat
        | A2 -> Not3Repeat
        | A3 -> NewSymbol
    | Not2Repeat ->
        match a with
        | A1 -> Not2Repeat
        | A2 -> NewSymbol
        | A3 -> Not2Repeat
    | Not1Repeat ->
        match a with
        | A1 -> NewSymbol
        | A2 -> Not1Repeat
        | A3 -> Not1Repeat
    | NewSymbol -> AfterAllSymbols
    | AfterAllSymbols -> AfterAllSymbols

let isFinal q =
    match q with
    | Only1
    | Only2
    | Only3
    | Not3Repeat
    | Not2Repeat
    | Not1Repeat 
    | AfterAllSymbols -> true
    | _ -> false

open CustomDfa

let Dfa =
    { Dfa4.Delta = delta
      Initial = Initial
      IsFinal = isFinal }
