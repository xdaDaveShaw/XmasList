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

let onFocus dispatch msg =
  OnFocus (fun (ev: Fable.Import.React.FocusEvent) ->
      ev.preventDefault()
      dispatch msg !!ev.target?value)

let itemListItem item =
  li [] [ str item.Description ]

let startAddItem dispatch child =
  Button.button [
    Button.OnClick (fun _ -> dispatch (UpdatingCurrent (Item (child, ""))))
  ] [ str "start adding items" ]

let addItems dispatch child item =
  div [ ] [
    Input.text [
      Input.OnChange (fun ev -> (child, !!ev.target?value) |> CurrentEntry.Item |> UpdatingCurrent |> dispatch)
      Input.Props [
        Props.AutoFocus true
        onEnter dispatch AddedItem
        Props.OnFocus (fun ev -> (child, !!ev.target?value) |> CurrentEntry.Item |> UpdatingCurrent |> dispatch)
      ]
      Input.Value item
    ]
    Button.button [
      Button.OnClick (fun _ -> dispatch AddedItem)
    ] [ str "add item" ]
  ]

let niceChild dispatch currentEntry (child : NiceChild) =

  let content =
    match currentEntry with
    | Item (c, item) when c.Name = child.Name -> addItems dispatch child item
    | _ -> startAddItem dispatch child

  div [ ] [
    ul [ ] (child.Items |> List.map itemListItem)
    content
  ]
let childListItem dispatch currentEntry child =

  let name = getChildName child

  let content =
    match child with
    | Nice c ->
      niceChild dispatch currentEntry c
    | Unknown c ->
      div [ ] [
        Button.button [
          Button.OnClick (fun _ -> dispatch (ReviewedChild (c, NaughtyOrNice.Nice)))
        ] [ str "nice" ]
        Button.button [
          Button.OnClick (fun _ -> dispatch (ReviewedChild (c, NaughtyOrNice.Naughty)))
        ] [ str "naughty" ]
      ]
    | Naughty _ -> div [ ] [ str "NAUGTY" ]

  li [ ] [
    str name
    content
  ]

let root model dispatch =

  let childListItem = childListItem dispatch model.CurrentEntry

  let value =
    match model.CurrentEntry with
    | Child c -> [ Input.Value c ]
    | _ -> []

  div [ ] [
    h1 [ ] [ str "List of Children" ]
    ul [ ]
      (model.ChildrensList
      |> List.map childListItem)

    h1 [ ] [ str "Add more Children" ]
    Input.text ([
      Input.OnChange (fun ev -> !!ev.target?value |> CurrentEntry.Child |> UpdatingCurrent |> dispatch)
      Input.Props [
        Props.AutoFocus true
        onEnter dispatch AddedChild
        Props.OnFocus (fun ev -> !!ev.target?value |> CurrentEntry.Child |> UpdatingCurrent |> dispatch)
      ]
    ] @ value)
    Button.button [
      Button.OnClick (fun _ -> dispatch AddedChild )
    ] [ str "add child" ]

    h1 [ ] [ str "Santa's List" ]
    ul [ ] (
      model.SantasList
      |> List.map (fun i -> li [ ] [ str (sprintf "%s * %d" i.Name i.Quantity) ])
    )
  ]
