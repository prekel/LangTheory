module LangTheoryFeliz.App.Main

open Feliz
open Browser.Dom
open Fable.Core.JsInterop

importAll "./styles/global.scss"

ReactDOM.render (App.App(), document.getElementById "feliz-app")
