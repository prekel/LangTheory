module LangTheoryFeliz.App.Lab03

open Feliz

open Lab03.Core


type PdaVar =
    | Sample35 of Set<Pda.State<Sample35.State, Sample35.Input, Sample35.Stack>> list option
    | Variant13Chapter1 of
        Set<Pda.State<Variant13Chapter1.State, Variant13Chapter1.Input, Variant13Chapter1.Stack>> list option
    | Variant13Chapter2 of
        Set<Pda.State<Variant13Chapter2.State, Variant13Chapter2.Input, Variant13Chapter2.Stack>> list option

type private Msg =
    | Update of string
    | SetCurrentPda of PdaVar

type private Model = { String: string; PdaVar: PdaVar }

let private update1 y a =
    let toSetOption pda charToAlphabet =
        y
        |> Seq.map charToAlphabet
        |> Seq.toList
        |> Pda.liftListOption
        |> Option.map (Pda.pdaSolve pda)

    match a.PdaVar with
    | Sample35 i ->
        toSetOption Sample35.pda Sample35.charToAlphabet
        |> Sample35
    | Variant13Chapter1 i ->
        toSetOption Variant13Chapter1.pda Variant13Chapter1.charToAlphabet
        |> Variant13Chapter1
    | Variant13Chapter2 i ->
        toSetOption Variant13Chapter2.pda Variant13Chapter2.charToAlphabet
        |> Variant13Chapter2

let private update a b =
    match b with
    | Update y -> { String = y; PdaVar = update1 y a }
    | SetCurrentPda y -> { String = ""; PdaVar = y }

let private init () = { String = ""; PdaVar = Sample35 None }

[<ReactComponent>]
let Lab03 () =
    let state, dispatch = React.useReducer (update, init ())

    let radioList name values =
        let element (value: string, text: string, f) =
            Html.div [ Html.input [ prop.type' "radio"
                                    prop.name name
                                    prop.value value
                                    prop.onCheckedChange (fun o -> if o then dispatch (SetCurrentPda f))
                                    prop.isChecked (
                                        match state.PdaVar, f with
                                        | Sample35 _, Sample35 _ -> true
                                        | Variant13Chapter1 _, Variant13Chapter1 _ -> true
                                        | Variant13Chapter2 _, Variant13Chapter2 _ -> true
                                        | _ -> false
                                    ) ]
                       Html.label [ prop.for' value
                                    prop.text text ]
                       Html.br [] ]

        values |> List.map element |> Html.div

    let boolToResult =
        function
        | false -> "Отвергнуто"
        | true -> "Принято"

    let listviz (a: Set<Pda.State<_, _, _>> list option) =
        match a with
        | Some a ->
            a
            |> List.map (fun s -> Html.p (sprintf "%A" s))
            |> Html.div
        | None -> Html.div []

    Html.div [ Html.p "Пример 35"
               Html.p "Чётный палиндром, алфавит 0 и 1"
               Html.br []

               Html.p "Вариант 13 (Часть 1)"
               Html.p "w_1cw_2 : w_1 != w_2^R, и w_1, w_2 принадлежат {a, b}*, алфавит {a, b, c}"
               Html.br []

               Html.p "Вариант 13 (Часть 2)"
               Html.p "w принадлежит {a, b, c}* : n_a(w) + n_b(w) != n_c(w), алфавит {a, b, c}"
               Html.br []

               radioList
                   "pda"
                   [ "Sample35", "Пример 35", (Sample35 None)
                     "Variant13Chapter1", "Вариант 13 (Часть 1)", (Variant13Chapter1 None)
                     "Variant13Chapter2", "Вариант 13 (Часть 2)", (Variant13Chapter2 None) ]

               Html.input [ prop.value state.String
                            prop.onChange (dispatch << Update) ]
               Html.br []

               match state.PdaVar with
               | Sample35 a -> listviz a
               | Variant13Chapter1 a -> listviz a
               | Variant13Chapter2 a -> listviz a
               Html.br []
               Html.p $"%A{state}" ]
