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

let itemListItem item =
  li [] [ str item.Description ]

let createStartAddButton dispatch child hasItems =

  let text =
    if hasItems then " + " else "start adding"

  let msg =
    Item (child, "") |> UpdatingCurrent

  Button.button [
    Button.Size Size.IsSmall
    Button.OnClick (fun _ -> dispatch msg)
  ] [ str text ]

let addItems dispatch child item =

  let updateCurrent s =
    Item (child, s) |> UpdatingCurrent |> dispatch

  Field.div [ Field.HasAddons ] [
    Input.text [
      Input.OnChange (fun ev -> updateCurrent !!ev.target?value)
      Input.Props [
        Props.AutoFocus true
        onEnter dispatch AddedItem
        Props.OnFocus (fun ev -> updateCurrent !!ev.target?value)
      ]
      Input.Value item
    ]
    Button.button [
      Button.Color Color.IsSuccess
      Button.OnClick (fun _ -> dispatch AddedItem)
    ] [ str "add item" ]
  ]

let renderNiceChild dispatch currentEntry child items =

  let hasItems = items |> List.isEmpty |> not

  let content =
    match currentEntry with
    | Item (c, item) when c.Name = child.Name -> addItems dispatch child item
    | _ -> createStartAddButton dispatch child hasItems

  tr [ ] [
    td [ ColSpan 3 ] [
      str child.Name
      ul [ ] (items |> List.map itemListItem)
      content
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
    td [ ] [ str child.Name ]
    td [ ] [ createButton (Nice []) ]
    td [ ] [ createButton Naughty ]
  ]

let renderChildListItem dispatch currentEntry child =

  let content =
    match child.NaughtyOrNice with
    | Nice items ->
      renderNiceChild dispatch currentEntry child items
    | Undecided ->
      renderUndecidedChild dispatch child
    | Naughty ->
      renderNaughtyChild child.Name

  content

let renderChildList dispatch model =

  let childList =
    model.ChildrensList
    |> List.map (fun c -> renderChildListItem dispatch model.CurrentEntry c)

  Content.content [ ] [
    if not (List.isEmpty model.ChildrensList) then
      yield Heading.h1 [ ] [ str "List of Children" ]
    yield
      Table.table [
        Table.IsFullWidth
      ] childList
  ]

let renderAddChild dispatch currentEntry =

  let value, autoFocus =
    match currentEntry with
    | Child c -> [ Input.Value c ], true
    | _ -> [], false

  let updatingCurrent s =
    Child s |> UpdatingCurrent |> dispatch

  let textProps =
    [
      Input.OnChange (fun ev -> updatingCurrent !!ev.target?value)
      Input.Props [
        Props.AutoFocus autoFocus
        onEnter dispatch AddedChild
        Props.OnFocus (fun ev -> updatingCurrent !!ev.target?value)
      ]
    ] @ value

  Content.content [ ] [
    Heading.h1 [ ] [ str "Add children" ]
    Field.div [ Field.HasAddons ] [
      Input.text textProps
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
    renderAddChild dispatch model.CurrentEntry
    renderSantasList model.SantasList
  ]
