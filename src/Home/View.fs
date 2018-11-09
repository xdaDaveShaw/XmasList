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
        Button.button [
          Button.Color IsPrimary
          Button.OnClick (fun _ -> dispatch AcceptName)
        ] [ str "Done" ] ] ]
  ]

let addItem model dispatch =
  match model.State with
  | ListState.Sent -> null
  | _ ->
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

let confirmButtons dispatch =
  Field.div [ Field.IsGrouped ] [
    Control.div [ ] [
      Button.button [
        Button.Color IsSuccess
        Button.OnClick (fun _ -> dispatch Send)
      ] [
        Icon.faIcon [] [ Fa.icon Fa.I.Check ]
        Text.span [] [ str "Confirm" ]
      ]
    ]
    Control.div [ ] [
      Button.button [
        Button.Color IsDanger
        Button.OnClick (fun _ -> dispatch CancelSend)
      ] [
        Icon.faIcon [ ] [ Fa.icon Fa.I.Times ]
        Text.span [ ] [ str "Cancel" ]
      ]
    ] ]

let sendButton state dispatch =
  let isSent =
    state = ListState.Sent

  let text =
    if isSent then "Sent" else "Send"

  Field.div [ ] [
    Control.div [ ] [
      Button.button [
        Button.OnClick (fun _ -> dispatch Sending)
        Button.Disabled isSent
      ] [ str text ]
    ] ]

let sendButtons model dispatch =
  match model.State with
  | ListState.Sending ->  confirmButtons dispatch
  | ListState.Unsent
  | ListState.Sent -> sendButton model.State dispatch
  | ListState.Unknown -> null

let knownUi model dispatch =
  [
    Field.div [ ] [
      Label.label [ Label.Size Size.IsLarge ] [ str ("Welcome " + model.Name) ]
      Label.label [ ] [
        Icon.faIcon [ ] [ Fa.icon Fa.I.Tree ]
        str "Your Xmas List"
        Icon.faIcon [ ] [ Fa.icon Fa.I.Tree ]
      ] ]
    Field.div [ ] [
      ul [ ]
        (model.Items
        |> List.map (fun i -> li [] [ str i.Description])) ]
    addItem model dispatch
    sendButtons model dispatch
  ]

let root model dispatch =

  let ui =
    match model.State with
    | Unknown -> unknownUi
    | _ -> knownUi
  let ui = ui model dispatch

  div [ ] [
    form [ ] ui
  ]
