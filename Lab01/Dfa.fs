module Dfa

type State =
    | Mod3Is0
    | Mod3Is1
    | Mod3Is2

type Alphabet =
    | A // a
    | B // b

let delta q (a: Alphabet) =
    match q with
    | Mod3Is0 -> Mod3Is1
    | Mod3Is1 -> Mod3Is2
    | Mod3Is2 -> Mod3Is0

let deltaViaTable q a =
    [ ((Mod3Is0, A), Mod3Is1)
      ((Mod3Is0, B), Mod3Is1)
      ((Mod3Is1, A), Mod3Is2)
      ((Mod3Is1, B), Mod3Is2)
      ((Mod3Is2, A), Mod3Is0)
      ((Mod3Is2, B), Mod3Is0) ]
    |> Map.ofList
    |> Map.find (q, a)

open Fa

let Dfa =
    { Dfa.Transition = delta
      Initial = Mod3Is0
      Final = Set.singleton Mod3Is0 }
