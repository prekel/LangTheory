module CustomDfa

let verifyCustomDfa start finish delta str =
    (start, str) ||> Seq.fold delta |> (=) finish

type Dfa0<'Q, 'A> =
    { Initial: 'Q
      Final: 'Q
      Delta: 'Q -> 'A -> 'Q }

let verifyCustomDfa1 dfa str =
    (dfa.Initial, str)
    ||> Seq.fold dfa.Delta
    |> (=) dfa.Final

// A<Q, Σ>
type Dfa<'Q, 'Sigma when 'Q: comparison> =
    { Delta: 'Q -> 'Sigma -> 'Q // δ
      Initial: 'Q // q0
      Final: 'Q Set } // F

let stateDfa dfa str =
    (dfa.Initial, str) ||> Seq.fold dfa.Delta


let verifyCustomDfa2 dfa str = stateDfa dfa str |> dfa.Final.Contains


// A<Q, Σ>
type Dfa4<'Q, 'Sigma> =
    { Delta: 'Q -> 'Sigma -> 'Q // δ
      Initial: 'Q // q0
      IsFinal: 'Q -> bool } // F

let stateDfa4 (dfa: Dfa4<_, _>) str =
    (dfa.Initial, str) ||> Seq.fold dfa.Delta

let verifyCustomDfa4 (dfa: Dfa4<_, _>) str = stateDfa4 dfa str |> dfa.IsFinal

// A<Q, Σ>
type Dfa5<'Q, 'Sigma> =
    { Delta: 'Q -> 'Sigma -> 'Q option // δ
      Initial: 'Q // q0
      IsFinal: 'Q -> bool } // F

let stateDfa5 (dfa: Dfa5<_, _>) str =
    (Some dfa.Initial, str)
    ||> Seq.fold (fun state t ->
            match state with
            | Some a -> dfa.Delta a t
            | None -> None)

let stateDfa5b (dfa: Dfa5<_, _>) str =
    (Some dfa.Initial, str)
    ||> Seq.fold (fun state t -> Option.bind (fun a -> dfa.Delta a t) state)

let stateDfa5b1 (dfa: Dfa5<_, _>) str =
    (Some dfa.Initial, str)
    ||> Seq.fold (fun state t -> Option.bind (fun a -> dfa.Delta a t) state)

type Result<'Q> =
    | Ok
    | NotReachedFinalState of 'Q
    | NotReachedEndOfSeq

let verifyCustomDfa5 (dfa: Dfa5<_, _>) str =
    match stateDfa5b dfa str with
    | Some q when dfa.IsFinal q -> Ok
    | Some q -> NotReachedFinalState q
    | None -> NotReachedEndOfSeq

// A<Q, Σ>
type Nfa<'State, 'Alphabet when 'State: comparison> =
    { Transition: 'State -> 'Alphabet -> 'State Set // δ
      Initial: 'State // q0
      Final: 'State Set } // F

//let stateNfa (nfa:Nfa<int, char>) (str: seq<char>) =
let stateNfa nfa str =
    (Set.singleton nfa.Initial, str)
    ||> Seq.fold (fun states current ->
            states
            |> Set.map (fun s -> nfa.Transition s current)
            |> Set.unionMany)

let verifyNfa nfa str =
    stateNfa nfa str
    |> Set.intersect nfa.Final
    |> Set.isEmpty
    |> (=) false
