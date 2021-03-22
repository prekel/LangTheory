module DFAPlayground.Pda

open System

// P<Q, Σ>
type Pda<'State, 'InputAlphabet, 'StackAlphabet when 'State: comparison> =
    { Transition: 'State -> 'InputAlphabet option -> 'StackAlphabet -> 'State Set * 'StackAlphabet list // δ(q, a, X) -> (p, γ)
      Initial: 'State // q0
      Final: 'State Set // F
      StackInitial: 'StackAlphabet } // Z0

let inline unimplemented () = raise <| NotImplementedException()

let newStack (stack: 'a list) (g: 'a list): 'a list =
    match g with
    | [] -> stack |> List.tail
    | [ x ] when x = (stack |> List.head) -> stack
    | [ x ] -> x :: (stack |> List.tail)
    | yz -> yz @ (stack |> List.tail)

type State<'State, 'InputAlphabet, 'StackAlphabet when 'State: comparison> =
    { Stack: 'StackAlphabet list
      State: 'State
      Str: 'InputAlphabet list }

let nextStates pda state =
    let j1 =
        match state.Str with
        | [] -> Set.empty
        | _ ->
            let (sts, q) =
                pda.Transition state.State (state.Str |> List.head |> Some) (state.Stack |> List.head)

            let j1 =
                sts
                |> Set.map (fun sa ->
                    { Stack = newStack state.Stack q
                      State = sa
                      Str = state.Str |> List.tail })

            j1

    let (sts, q) =
        pda.Transition state.State None (state.Stack |> List.head)

    let j2 =
        sts
        |> Set.map (fun sa ->
            { Stack = newStack state.Stack q
              State = sa
              Str = state.Str })

    j1 |> Set.union j2

let pdaSteps1 pda init =
    let rec statesRec states =
        let s =
            states
            |> List.head
            |> Set.map (nextStates pda)
            |> Set.unionMany

        match s |> Set.isEmpty with
        | true -> states
        | _ -> statesRec (s :: states)

    statesRec ([ Set.singleton init ])

let rec pdaSteps pda str =

    let pdastep state a stack =
        let (newState, s) = pda.Transition state a pda.StackInitial
        let ns = newStack stack s
        newState, stack

    let a =
        pda.Transition pda.Initial (str |> List.head |> Some) pda.StackInitial

    let b =
        pda.Transition pda.Initial None pda.StackInitial

    let rec steps (a: Set<State<'a, 'b, 'c>>): Set<State<'a, 'b, 'c>> =
        let u =
            a
            |> Set.map (fun s ->
                let (sts, q) =
                    pdastep s.State (s.Str |> List.head |> Some) s.Str

                sts
                |> Set.map (fun sa ->
                    { Stack = q
                      State = sa
                      Str = str |> List.tail }))

        unimplemented ()

    let init =
        Set.singleton
            { Stack = [ pda.StackInitial ]
              State = pda.Initial
              Str = str }

    let s1 =
        { Stack = newStack (init |> Set.minElement).Stack (a |> snd)
          State = a |> fst
          Str = str |> List.tail }

    let res =
        List.fold (fun s y ->

            unimplemented ()) init str

    unimplemented ()

module Sample35 =

    type State =
        | Q0
        | Q1
        | Q2

    type Input =
        | In0
        | In1

    type Stack =
        | St0
        | St1
        | StZ0

    let delta q (a: Input option) (X: Stack) =
        match q, a, X with
        | Q0, Some In0, StZ0 -> Set.singleton Q0, [ St0; StZ0 ]
        | Q0, Some In1, StZ0 -> Set.singleton Q0, [ St1; StZ0 ]
        | Q0, Some In0, St0 -> Set.singleton Q0, [ St0; St0 ]
        | Q0, Some In0, St1 -> Set.singleton Q0, [ St0; St1 ]
        | Q0, Some In1, St0 -> Set.singleton Q0, [ St1; St0 ]
        | Q0, Some In1, St1 -> Set.singleton Q0, [ St1; St1 ]
        | Q0, None, StZ0 -> Set.singleton Q1, [ StZ0 ]
        | Q0, None, St0 -> Set.singleton Q1, [ St0 ]
        | Q0, None, St1 -> Set.singleton Q1, [ St1 ]
        | Q1, Some In0, St0 -> Set.singleton Q1, []
        | Q1, Some In1, St1 -> Set.singleton Q1, []
        | Q1, None, StZ0 -> Set.singleton Q2, [ StZ0 ]
        | _ -> Set.empty, []

    let charToAlphabet =
        function
        | '0' -> In0
        | '1' -> In1
        | _ -> failwith ""

    let pda =
        { Pda.Transition = delta
          Initial = Q0
          Final = Set.singleton Q2
          StackInitial = StZ0 }

    let init str =
        { Stack = [ StZ0 ]
          State = Q0
          Str = str }
