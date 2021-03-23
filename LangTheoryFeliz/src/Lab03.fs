module LangTheoryFeliz.App.Lab03

open Feliz

open Lab03.Core

type private State = { Pda: string }

type private Msg = Update of string

let private update a b =
    match b with
    | Update y ->
        try
            let l =
                y
                |> Seq.map (Sample35.charToAlphabet)
                |> Seq.toList

            Pda.pdaSolve Sample35.pda l |> Some
        with ex -> None

[<ReactComponent>]
let listviz a =
    match a with
    | Some a ->
        a
        |> List.map (fun s -> Html.p (sprintf "%A" s))
        |> Html.div
    | None -> Html.div []


[<ReactComponent>]
let Lab03 () =
    let state, dispatch = React.useReducer (update, None)

    Html.div [ Html.p "Пример 35"
               Html.br []
               Html.p "Чётный палиндром, алфавит 0 и 1"

               Html.input [ prop.onChange (dispatch << Update) ]
               Html.br []
               match state with
               | Some a -> Html.p ((Pda.pdaCheck1 Sample35.pda a) |> string)
               | None -> Html.p "none"
               Html.p (listviz state)
               Html.br [] ]
