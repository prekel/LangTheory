module CustomDfa

let verifyCustomDfa start finish delta str =
    (start, str) ||> Seq.fold delta |> (=) finish

type Dfa<'Q, 'A> =
    { Initial: 'Q
      Final: 'Q
      Delta: 'Q -> 'A -> 'Q }

let verifyCustomDfa1 dfa str =
    (dfa.Initial, str)
    ||> Seq.fold dfa.Delta
    |> (=) dfa.Final
