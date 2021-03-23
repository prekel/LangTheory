module LangTheoryFeliz.App.Lab03

open Feliz

open Lab01.Core

type DfaState =
    { DfaString: string
      DfaSeq: seq<Dfa.Alphabet> option
      DfaState: Dfa.State option
      DfaIsFinal: bool option }

type NfaState =
    { NfaString: string
      NfaSeq: seq<Nfa.Alphabet> option
      NfaStates: Nfa.State Set option
      NfaIsFinal: bool option }

type DfaResult =
    | DfaState of DfaState
    | DfaError of string

type NfaResult =
    | NfaState of NfaState
    | NfaError of string

type State = { Dfa: DfaResult; Nfa: NfaResult }

type private Msg =
    | UpdateDfa of string
    | UpdateNfa of string

let proceedDfa str =
    try
        let dfa = Dfa.Dfa
        let sq = str |> Seq.map Dfa.charToAlphabet
        let state = Fa.stateDfa dfa sq
        let isFinal = Fa.verifyDfa dfa sq

        DfaState
            { DfaString = str
              DfaSeq = Some sq
              DfaState = Some state
              DfaIsFinal = Some isFinal }
    with ex -> DfaError ex.Message


let proceedNfa str =
    try
        let nfa = Nfa.Nfa
        let sq = str |> Seq.map Nfa.charToAlphabet
        let state = Fa.stateNfa nfa sq
        let isFinal = Fa.verifyNfa nfa sq

        NfaState
            { NfaString = str
              NfaSeq = Some sq
              NfaStates = Some state
              NfaIsFinal = Some isFinal }
    with ex -> NfaError ex.Message

let private update a b =
    match b with
    | UpdateDfa y -> { a with Dfa = proceedDfa y }
    | UpdateNfa y -> { a with Nfa = proceedNfa y }

[<ReactComponent>]
let Lab01 () =
    let state, dispatch =
        React.useReducer
            (update,
             { Dfa = proceedDfa ""
               Nfa = proceedNfa "" })

    Html.div [ Html.p "Вариант 14."
               Html.br []
               Html.p "а) Построить ДКА, допускающий в алфавите {a, b} все строки, длина которых нацело делится на 3."
               Html.p
                   "б) Построить НКА, допускающий язык из цепочек из 0 и 1, которые содержат ровно две единицы и по крайней мере два нуля."
               Html.br []

               Html.p "а"
               Html.input [ prop.onChange (dispatch << UpdateDfa) ]
               Html.br []
               Html.p (sprintf "%A" state.Dfa)
               Html.br []

               Html.p "б"
               Html.input [ prop.onChange (dispatch << UpdateNfa) ]
               Html.br []
               Html.p (sprintf "%A" state.Nfa)
               Html.br [] ]
