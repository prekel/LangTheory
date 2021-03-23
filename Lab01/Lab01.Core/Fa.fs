module Lab01.Core.Fa

// A<Q, Σ>
type Dfa<'State, 'Alphabet when 'State: comparison> =
    { Transition: 'State -> 'Alphabet -> 'State // δ
      Initial: 'State // q0
      Final: 'State Set } // F

let stateDfa dfa str =
    (dfa.Initial, str) ||> Seq.fold dfa.Transition

let verifyDfa dfa str = stateDfa dfa str |> dfa.Final.Contains

// A<Q, Σ>
type Nfa<'State, 'Alphabet when 'State: comparison> =
    { Transition: 'State -> 'Alphabet -> 'State Set // δ
      Initial: 'State // q0
      Final: 'State Set } // F

let stateNfa nfa str =
    (Set.singleton nfa.Initial, str)
    ||> Seq.fold (fun states current ->
            states
            |> Seq.map (fun s -> nfa.Transition s current)
            |> Set.unionMany)

let verifyNfa nfa str =
    stateNfa nfa str
    |> Set.intersect nfa.Final
    |> Set.isEmpty
    |> (=) false
