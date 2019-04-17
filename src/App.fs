module XmasList.App

open Elmish
open Fable.Core.JsInterop
open State
open View

importAll "../sass/main.sass"

open Elmish.React
open Elmish.Debug
open Elmish.HMR //Must be last

// App
Program.mkProgram init update root
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
