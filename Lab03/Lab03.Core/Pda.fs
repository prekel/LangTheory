module Lab03.Core.Pda

/// P<Q, Σ, Γ>
type Pda<'State, 'InputAlphabet, 'StackAlphabet when 'State: comparison> =
    { /// δ(q, a, X) -> (p, γ)
      Transition: 'State -> 'InputAlphabet option -> 'StackAlphabet -> 'State Set * 'StackAlphabet list
      /// q0
      Initial: 'State
      /// F
      Final: 'State Set
      /// Z0
      StackInitial: 'StackAlphabet }

type SuperState<'State, 'InputAlphabet, 'StackAlphabet> =
    { Stack: 'StackAlphabet list
      State: 'State
      Str: 'InputAlphabet list }

/// Pda<'a,'b,'c> -> SuperState<'a,'b,'c> -> Set<SuperState<'a,'b,'c>>
let nextStates pda state =
    let nextStates1 sym =
        let newStack stack g =
            match g with
            | [] -> stack |> List.tail
            | [ x ] when x = (stack |> List.head) -> stack
            | [ x ] -> x :: (stack |> List.tail)
            | yz -> yz @ (stack |> List.tail)

        let sts, q =
            pda.Transition state.State sym (state.Stack |> List.head)

        sts
        |> Set.map
            (fun sa ->
                { Stack = newStack state.Stack q
                  State = sa
                  Str =
                      match sym with
                      | Some _ -> state.Str |> List.tail
                      | None -> state.Str })

    match state.Stack with
    | [] -> Set.empty
    | _ ->
        nextStates1 None |> Set.union
        <| match state.Str with
           | [] -> Set.empty
           | _ -> nextStates1 (state.Str |> List.head |> Some)

/// Pda<'a,'b,'c> -> 'b list -> Set<SuperState<'a,'b,'c>> list
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

/// Pda<'a,'b,'c> -> SuperState<'a,'d,'e> -> bool
let checkFunFinalState pda t =
    match Set.contains t.State pda.Final, List.isEmpty t.Str with
    | true, true -> true
    | _ -> false

/// Pda<'a,'b,'c> -> Set<SuperState<'a,'d,'e>> list -> bool
let pdaCheck1 pda state =
    state
    |> List.map (Set.exists (checkFunFinalState pda))
    |> List.contains true

/// Pda<'a,'b,'c> -> 'b list -> bool
let pdaCheck pda str = pdaSolve pda str |> pdaCheck1 pda

/// 'a option list -> 'a list option
let liftListOption optionList =
    match optionList |> List.contains None with
    | true -> None
    | false -> optionList |> List.map Option.get |> Some
