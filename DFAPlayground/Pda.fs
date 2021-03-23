module DFAPlayground.Pda

open System

let inline unimplemented () = raise <| NotImplementedException()

// P<Q, Σ, Γ>
type Pda<'State, 'InputAlphabet, 'StackAlphabet when 'State: comparison> =
    { Transition: 'State -> 'InputAlphabet option -> 'StackAlphabet -> 'State Set * 'StackAlphabet list // δ(q, a, X) -> (p, γ)
      Initial: 'State // q0
      Final: 'State Set // F
      StackInitial: 'StackAlphabet } // Z0

type State<'State, 'InputAlphabet, 'StackAlphabet> =
    { Stack: 'StackAlphabet list
      State: 'State
      Str: 'InputAlphabet list }

let nextStates pda state =
    let nextStates1 sym =
        let newStack stack g =
            match g with
            | [] -> stack |> List.tail
            | [ x ] when x = (stack |> List.head) -> stack
            | [ x ] -> x :: (stack |> List.tail)
            | yz -> yz @ (stack |> List.tail)

        let (sts, q) =
            pda.Transition state.State sym (state.Stack |> List.head)

        sts
        |> Set.map (fun sa ->
            { Stack = newStack state.Stack q
              State = sa
              Str =
                  match sym with
                  | Some _ -> state.Str |> List.tail
                  | None -> state.Str })

    nextStates1 None |> Set.union
    <| match state.Str with
       | [] -> Set.empty
       | _ -> nextStates1 (state.Str |> List.head |> Some)

let pdaSolve pda str =
    let rec statesRec states =
        let s =
            states
            |> List.head
            |> Set.map (nextStates pda)
            |> Set.unionMany

        match s |> Set.isEmpty with
        | true -> states
        | _ -> statesRec (s :: states)

    { Stack = [ pda.StackInitial ]
      State = pda.Initial
      Str = str }
    |> Set.singleton
    |> List.singleton
    |> statesRec

let pdaCheck pda str =
    pdaSolve pda str
    |> List.head
    |> Set.filter (fun t -> Set.contains t.State pda.Final)
    |> Set.isEmpty
    |> not
