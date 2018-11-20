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

let niceChild dispatch currentEntry child items =

  let content =
    match currentEntry with
    | Item (c, item) when c.Name = child.Name -> addItems dispatch child item
    | _ -> startAddItem dispatch child

  div [ ] [
    ul [ ] (items |> List.map itemListItem)
    content
  ]
let renderChildListItem dispatch currentEntry child =

  let content =
    match child.NaughtyOrNice with
    | Nice items ->
      niceChild dispatch currentEntry child items
    | Undecided ->
      div [ ] [
        Button.button [
          Button.OnClick (fun _ -> dispatch (ReviewedChild (child, Nice [])))
        ] [ str "nice" ]
        Button.button [
          Button.OnClick (fun _ -> dispatch (ReviewedChild (child, Naughty)))
        ] [ str "naughty" ]
      ]
    | Naughty _ -> div [ ] [ str "NAUGTY" ]

  li [ ] [
    str child.Name
    content
  ]

let renderChildList dispatch model =

  let childList =
    model.ChildrensList
    |> List.map (fun c -> renderChildListItem dispatch model.CurrentEntry c)

  Content.content [ ] [
    Heading.h1 [ ] [ str "List of Children" ]
    ul [ ] childList
  ]

let renderAddItem dispatch currentEntry =

  let value =
    match currentEntry with
    | Child c -> [ Input.Value c ]
    | _ -> []

  let updatingCurrent s =
    Child s |> UpdatingCurrent |> dispatch

  let textProps =
    [
      Input.OnChange (fun ev -> updatingCurrent !!ev.target?value)
      Input.Props [
        Props.AutoFocus true
        onEnter dispatch AddedChild
        Props.OnFocus (fun ev -> updatingCurrent !!ev.target?value)
      ]
    ] @ value

  Content.content [ ] [
    Heading.h1 [ ] [ str "Add children" ]
    Input.text textProps
    Button.button [
      Button.OnClick (fun _ -> dispatch AddedChild )
    ] [ str "add child" ]
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
    renderAddItem dispatch model.CurrentEntry
    renderSantasList model.SantasList
  ]
