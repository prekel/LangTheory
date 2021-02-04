module Lab01

open Feliz

[<ReactComponent>]
let HelloWorld () =
    let dfaInput, setDfaInput = React.useState ""
    let nfa, setNfaInput = React.useState ""

    Html.div [ Html.input [ prop.onChange setDfaInput ]
               Html.input [ prop.onChange setNfaInput ] ]
