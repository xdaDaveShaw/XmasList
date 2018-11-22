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

let createStartAddButton dispatch child =
  let msg =
    (child, "") |> UpdatingItem

  a [
    OnClick (fun _ -> dispatch msg)
  ] [ str "add items"]

let createInputButtonCombo
  dispatch
  value
  buttonText
  onChange
  onCommitMsg
  additionalInputProps =
  Field.div [ Field.HasAddons ] [
    Input.text [
      Input.OnChange (fun ev -> onChange !!ev.target?value)
      Input.Props ([
        Props.AutoFocus true
        onEnter dispatch onCommitMsg
        Props.OnFocus (fun ev -> onChange !!ev.target?value)
      ] @ additionalInputProps)
      Input.Value value
    ]
    Button.button [
      Button.Color Color.IsSuccess
      Button.OnClick (fun _ -> dispatch onCommitMsg)
    ] [ str buttonText ]
  ]

let createAddNewItemControl dispatch child item =

  let updateCurrent s =
    (child, s) |> UpdatingItem |> dispatch

  let onBlur =
    (Props.OnBlur (fun _ -> dispatch EndedUpdatingItem))

  createInputButtonCombo
    dispatch
    item
    "add item"
    updateCurrent
    AddedItem
    [ onBlur ]

let renderNiceChild dispatch currentItem child items =

  let itemList =
    items
    |> List.map (fun item -> li [] [ str item.Description ])

  let addControl =
    match currentItem with
    | Some (c, item) when c.Name = child.Name -> createAddNewItemControl dispatch child item
    | _ -> li [ ] [ createStartAddButton dispatch child ]

  tr [ ] [
    td [ ColSpan 3 ] [
      strong [ ] [ str child.Name ]
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
      yield Heading.h3 [ ] [ str "List of Children" ]

    yield
      Table.table [
        Table.IsFullWidth
        Table.IsBordered
        Table.IsStriped
      ] [
        tbody [ ] childList
      ]
  ]

let renderAddChild dispatch model =

  let autoFocus =
    model.CurrentEditor.CurrentItem |> Option.isNone

  let updatingCurrent s =
    s |> UpdatingChild |> dispatch

  Content.content [ ] [
    yield
      Heading.h3 [ ] [ str "Add children" ]

    if model.ChildrensList |> List.isEmpty then
      yield
        Heading.h5 [ Heading.IsSubtitle ] [ str "Add some children to the list to get started"]

    yield
      createInputButtonCombo
        dispatch
        model.CurrentEditor.EditingChildName
        "add"
        updatingCurrent
        AddedChild
        []
  ]

let renderSantasList list =

  let renderItem item =
    let text = sprintf "%s * %d" item.ItemName item.Quantity
    li [ ] [ str text ]

  let items =
    list
    |> List.map renderItem

  if items |> List.isEmpty then
    nothing
  else
    Content.content [ ] [
      Heading.h3 [ ] [ str "Santa's List" ]
      ul [ ] items
    ]

let root model dispatch =
  div [ ] [

    Navbar.navbar [ Navbar.Color Color.IsPrimary ] [
      Navbar.Brand.a [ ] [
        Image.image [ Image.Is48x48 ] [
          img [ Src "img/brand.png" ]
        ]
      ]
      Navbar.Item.div [ ] [
        Heading.h2 [ ] [ str "Santa's Xmas Manager"]
      ]
    ]

    Container.container [ ] [
      renderAddChild dispatch model
      renderChildList dispatch model
      renderSantasList model.SantasList
    ]
  ]
