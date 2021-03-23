namespace Lab03.Core

module Fa =
    // P<Q, Σ>
    type Pda<'State, 'InputAlphabet, 'StackAlphabet when 'State: comparison> =
        { Transition: 'State -> 'InputAlphabet option -> 'StackAlphabet -> 'State Set * 'StackAlphabet seq // δ(q, a, X) -> (p, γ)
          Initial: 'State // q0
          Final: 'State Set // F
          StackInitial: 'StackAlphabet } // Z0
