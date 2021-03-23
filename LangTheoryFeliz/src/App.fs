module LangTheoryFeliz.App.App

open Feliz
open LangTheoryFeliz.App

open Feliz.Router

type State = { CurrentUrl: string list }
type Msg = UrlChanged of string list

let init () = { CurrentUrl = Router.currentUrl () }

let update state (UrlChanged segments) = { state with CurrentUrl = segments }

[<ReactComponent>]
let Home () =
    Html.div [ Html.a [ prop.href (Router.format ("Lab01"))
                        prop.text "Lab01" ]
               Html.br []
               Html.a [ prop.href (Router.format ("Lab03"))
                        prop.text "Lab03" ] ]

[<ReactComponent>]
let Router () =
    let state, dispatch = React.useReducer (update, init ())

    React.router [ router.children [ match state.CurrentUrl with
                                     | [] -> Home()
                                     | [ "Lab01" ] -> Lab01.Lab01()
                                     | [ "Lab03" ] -> Lab03.Lab03()
                                     | _ -> Html.h1 "Not found" ] ]

[<ReactComponent>]
let App () = Html.div [ Router() ]
