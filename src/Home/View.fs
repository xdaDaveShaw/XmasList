module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Types

let unknownUi model dispatch =
  [
    Field.div [ ] [
      Control.div [ ] [
        Input.text [
          Input.Placeholder "Enter your name"
          Input.Props [ Props.AutoFocus true ]
          Input.OnChange (fun ev -> !!ev.target?value |> UpdatingName |> dispatch)
          Input.DefaultValue model.Name
        ]
      ]
    ]
    Field.div [ Field.IsGrouped ]
      [ Control.div [ ] [
        Button.Input.submit
          [ Button.Color IsPrimary
            Button.OnClick (fun _ -> dispatch AcceptName)
            Button.Props [ Value "Done" ] ] ] ]
  ]

let knownUi model dispatch =
  [
    Field.div [ ] [
      Label.label [ Label.Size Size.IsLarge ] [ str ("Welcome " + model.Name) ]
      Label.label [ ] [ str "Start adding to your Xmas List" ] ]
    Field.div [ ] [
      ul [ ]
        (model.Items
        |> List.map (fun i -> li [] [ str i.Description])) ]
    Field.div [ Field.HasAddons ] [
      Control.div [ Control.IsExpanded ] [
        Input.text [
          Input.Value model.Current.Description
          Input.OnChange (fun ev -> !!ev.target?value |> UpdatingCurrent |> dispatch)
          Input.Placeholder "What do you want?"
          Input.Props [ Props.AutoFocus true ] ]
      ]
      Control.div [ ] [
        Button.button [
          Button.OnClick (fun _ -> dispatch Add)
        ] [ str "Add" ]
      ] ]
    Field.div [ ] [
      Control.div [ ] [
        Button.button [
          Button.OnClick (fun _ -> dispatch Send)
        ] [ str "Send" ]
      ] ]
  ]

let root model dispatch =

  let ui =
    match model.IsAccepted with
    | false -> unknownUi
    | true -> knownUi
  let ui = ui model dispatch

  div [ ] [
    form [ ] ui
  ]
