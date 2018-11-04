module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Types

let root model dispatch =
  div [ ] [
    form [ ] [
      Field.div [ ] [
        Control.div [ ] [
          Input.text [
            Input.Placeholder "Enter your name"
            Input.Props [ Props.AutoFocus true ]
            Input.OnChange (fun ev -> !!ev.target?value |> ChangeStr |> dispatch)
            Input.DefaultValue model.Name
          ]
        ]
      ]
      Field.div [ Field.IsGrouped ]
        [ Control.div [ ] [
          Button.Input.submit
            [ Button.Color IsPrimary
              Button.OnClick (fun _ -> dispatch AcceptName)
              Button.Props [ Value "Done" ] ]
            ] ] ] ]
