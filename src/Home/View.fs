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
      if (not model.Accepted) then
        yield Field.div [ ] [
          Control.div [ ] [
            Input.text [
              Input.Placeholder "Enter your name"
              Input.Props [ Props.AutoFocus true ]
              Input.OnChange (fun ev -> !!ev.target?value |> ChangeStr |> dispatch)
              Input.IsReadOnly model.Accepted
              Input.DefaultValue model.Name
            ]
          ]
        ]
        yield Field.div [ Field.IsGrouped ]
          [ Control.div [ ] [
            Button.Input.submit
              [ Button.Color IsPrimary
                Button.OnClick (fun _ -> dispatch AcceptName)
                Button.Props [ Value "Done" ] ] ] ]
      else
        yield Field.div [ ] [
              Label.label [ Label.Size Size.IsLarge ] [ str ("Welcome " + model.Name) ]
              Label.label [ ] [ str "Start adding to your Xmas List" ] ]
        yield Field.div [ Field.HasAddons ] [
              Control.div [ Control.IsExpanded ] [
                Input.text [ Input.Placeholder "What do you want?" ]
              ]
              Control.div [ ] [
                Button.button [] [ str "Add"]
              ] ]
    ] ]
