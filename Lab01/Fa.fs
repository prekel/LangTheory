module Fa

// A<Q, Σ>
type Dfa<'State, 'Alphabet when 'State: comparison> =
    { Transition: 'State -> 'Alphabet -> 'State // δ
      Initial: 'State // q0
      Final: 'State Set } // F

let stateDfa dfa str =
    (dfa.Initial, str) ||> Seq.fold dfa.Transition

let verifyCustomDfa dfa str = stateDfa dfa str |> dfa.Final.Contains

// A<Q, Σ>
type Nfa<'State, 'Alphabet when 'State: comparison> =
    { Transition: 'State -> 'Alphabet -> 'State Set // δ
      Initial: 'State // q0
      Final: 'State Set } // F

