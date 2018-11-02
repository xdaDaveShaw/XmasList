module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

let root model dispatch =

  div
    [ ]
    [ p
        [ ClassName "control" ]
        [ input
            [ ClassName "input"
              Type "text"
              Placeholder "Type your name"
              DefaultValue model.Name
              AutoFocus true
              ReadOnly model.Accepted
              OnChange (fun ev -> !!ev.target?value |> ChangeStr |> dispatch ) ]
          a
            [ ClassName "button"
              OnClick (fun _ -> dispatch AcceptName ) ]
            [ str "Done" ]
        ]
      br [ ]
      span
        [ ]
        [ str (sprintf "Hello %s" model.Name) ] ]
