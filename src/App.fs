module XmasList.App

open Elmish
open Fable.Core.JsInterop
open State
open View

importAll "../sass/main.sass"

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
