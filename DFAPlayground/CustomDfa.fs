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
