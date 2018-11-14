module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome
open Types

let onEnter dispatch msg =
  OnKeyDown (fun (ev: Fable.Import.React.KeyboardEvent) ->
      match ev with
      | _ when ev.keyCode = 13. ->
          ev.preventDefault()
          dispatch msg

      | _ -> ())

let unknownUi model dispatch =
  [
    Field.div [ ] [
      Control.div [ ] [
        Input.text [
          Input.Placeholder "Enter your name"
          Input.Props [
            Props.AutoFocus true
            onEnter dispatch AcceptedName
          ]
          Input.OnChange (fun ev -> !!ev.target?value |> UpdatingName |> dispatch)
          Input.DefaultValue model.Name
        ]
      ]
    ]
    Field.div [ Field.IsGrouped ]
      [ Control.div [ ] [
        Button.button [
          Button.Color IsPrimary
          Button.OnClick (fun _ -> dispatch AcceptedName)
        ] [ str "Done" ] ] ]
  ]

let listItems items dispatch =

  let item i =
    Columns.columns [ Columns.IsMobile; Columns.CustomClass "gridcols" ] [
      Column.column [ Column.CustomClass "gridcol"; Column.Width (Screen.All, Column.Is11) ] [
        Icon.faIcon [ Icon.Size Size.IsSmall ] [
          Fa.icon Fa.I.Circle
        ]
        Text.span [ CustomClass "griditem"; ] [
          str i.Description
        ]
      ]
      Column.column [ Column.CustomClass "gridcol"; Column.Width (Screen.All, Column.Is1) ] [
        Delete.delete [ Delete.Size Size.IsSmall; Delete.CustomClass "griddel" ] [ ]
      ]
    ]


  let items =
    items
    |> List.map item

  Field.div [ ]
    items

let addItem model dispatch =
  match model.State with
  | ListState.Sent -> null
  | _ ->
    Field.div [ Field.HasAddons ] [
      Control.div [ ] [
        Input.text [
          Input.Value model.Current.Description
          Input.OnChange (fun ev -> !!ev.target?value |> UpdatingCurrent |> dispatch)
          Input.Placeholder "What do you want?"
          Input.CustomClass "add-item"
          Input.Props [
            Props.AutoFocus true
            onEnter dispatch AddCurrentToList
          ] ]
      ]
      Control.div [ ] [
        Button.button [
          Button.OnClick (fun _ -> dispatch AddCurrentToList)
          Button.Color Color.IsSuccess
        ] [
          Text.span [] [ str "Add" ]
          Icon.faIcon [] [ Fa.icon Fa.I.Check ]
        ]
      ] ]

let confirmButtons dispatch =
  Field.div [ Field.IsGrouped ] [
    Control.div [ ] [
      Button.button [
        Button.Color IsSuccess
        Button.OnClick (fun _ -> dispatch Sent)
      ] [
        Icon.faIcon [] [ Fa.icon Fa.I.Check ]
        Text.span [] [ str "Confirm" ]
      ]
    ]
    Control.div [ ] [
      Button.button [
        Button.Color IsDanger
        Button.OnClick (fun _ -> dispatch CancelledSend)
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
    br []
    listItems model.Items dispatch
    br []
    addItem model dispatch
    br []
    sendButtons model dispatch
  ]

let root model dispatch =

  let ui =
    match model.State with
    | Unknown -> unknownUi
    | _ -> knownUi
  let ui = ui model dispatch

  div [ ] ui
