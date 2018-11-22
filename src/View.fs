module XmasList.View

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

let itemListItem item =
  li [] [ str item.Description ]

let createStartAddButton dispatch child =
  let msg =
    (child, "") |> UpdatingItem

  a [
    OnClick (fun _ -> dispatch msg)
  ] [ str "add items"]

let createAddNewItemControl dispatch child item =

  let updateCurrent s =
    (child, s) |> UpdatingItem |> dispatch

  Field.div [ Field.HasAddons ] [
    Input.text [
      Input.OnChange (fun ev -> updateCurrent !!ev.target?value)
      Input.Props [
        Props.AutoFocus true
        onEnter dispatch AddedItem
        Props.OnFocus (fun ev -> updateCurrent !!ev.target?value)
        Props.OnBlur (fun _ -> dispatch EndedUpdatingItem)
      ]
      Input.Value item
    ]
    Button.button [
      Button.Color Color.IsSuccess
      Button.OnClick (fun _ -> dispatch AddedItem)
    ] [ str "add item" ]
  ]

let renderNiceChild dispatch currentItem child items =

  let itemList =
    items
    |> List.map itemListItem

  let addControl =
    match currentItem with
    | Some (c, item) when c.Name = child.Name -> createAddNewItemControl dispatch child item
    | _ -> li [ ] [ createStartAddButton dispatch child ]

  tr [ ] [
    td [ ColSpan 3 ] [
      str child.Name
      ul [ ] (itemList @ [ addControl ])
    ]
  ]

let renderNaughtyChild name =
  tr [
  ] [
    td [
      ColSpan 3
    ] [ str (name + " has been naughty.") ]
  ]

let renderUndecidedChild dispatch child =

  let createButton non =
    let text, col =
      match non with
      | Nice _ -> "nice", Color.IsSuccess
      | Naughty -> "naughty", Color.IsDanger
      | Undecided -> "", Color.IsBlack

    Control.p [ ] [
      Button.button [
        Button.Color col
        Button.Size Size.IsSmall
        Button.OnClick (fun _ -> dispatch (ReviewedChild (child, non)))
      ] [ str (text) ]
    ]

  tr [ ] [
    td [ Props.HTMLAttr.Width "100%" ] [ str child.Name ]
    td [ ] [ createButton (Nice []) ]
    td [ ] [ createButton Naughty ]
  ]

let renderChildListItem dispatch currentItem child =

  let content =
    match child.NaughtyOrNice with
    | Nice items ->
      renderNiceChild dispatch currentItem child items
    | Undecided ->
      renderUndecidedChild dispatch child
    | Naughty ->
      renderNaughtyChild child.Name

  content

let renderChildList dispatch model =

  let childList =
    model.ChildrensList
    |> List.map (fun c -> renderChildListItem dispatch model.CurrentEditor.CurrentItem c)

  Content.content [ ] [
    if not (List.isEmpty model.ChildrensList) then
      yield Heading.h1 [ ] [ str "List of Children" ]

    yield
      Table.table [
        Table.IsFullWidth
      ] [
        tbody [ ] childList
      ]
  ]

let renderAddChild dispatch currentEditor =

  let autoFocus =
    currentEditor.CurrentItem |> Option.isSome

  let updatingCurrent s =
    s |> UpdatingChild |> dispatch

  Content.content [ ] [
    Heading.h1 [ ] [ str "Add children" ]
    Field.div [ Field.HasAddons ] [
      Input.text [
        Input.OnChange (fun ev -> updatingCurrent !!ev.target?value)
        Input.Props [
          Props.AutoFocus autoFocus
          Props.OnFocus (fun ev -> updatingCurrent !!ev.target?value)
          onEnter dispatch AddedChild
        ]
        Input.Value currentEditor.EditingChildName
      ]
      Button.button [
        Button.Color Color.IsSuccess
        Button.OnClick (fun _ -> dispatch AddedChild )
      ] [ str "add" ]
    ]
  ]

let renderSantasList list =

  let renderItem item =
    let text = sprintf "%s * %d" item.ItemName item.Quantity
    li [ ] [ str text ]

  let items =
    list
    |> List.map renderItem

  Content.content [ ] [
    Heading.h1 [ ] [ str "Santa's List" ]
    ul [ ] items
  ]

let root model dispatch =

  div [ ] [
    renderChildList dispatch model
    renderAddChild dispatch model.CurrentEditor
    renderSantasList model.SantasList
  ]
