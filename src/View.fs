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

type InputAndButton<'a> = {
  Dispatch: Msg -> unit
  InputValue: string
  ButtonText: string
  PlaceholderText: string
  OnChange: 'a -> unit
  OnCommitMsg: Msg
  AdditionalInputProps: IHTMLProp list
}

let createInputButtonCombo
  inputAndButton =
  Field.div [ Field.HasAddons ] [
    Input.text [
      Input.Placeholder inputAndButton.PlaceholderText
      Input.OnChange (fun ev -> inputAndButton.OnChange !!ev.target?value)
      Input.Props ([
        Props.AutoFocus true
        onEnter inputAndButton.Dispatch inputAndButton.OnCommitMsg
        Props.OnFocus (fun ev -> inputAndButton.OnChange !!ev.target?value)
      ] @ inputAndButton.AdditionalInputProps)
      Input.Value inputAndButton.InputValue
    ]
    Button.button [
      Button.Color Color.IsSuccess
      Button.OnClick (fun _ -> inputAndButton.Dispatch inputAndButton.OnCommitMsg)
    ] [ str inputAndButton.ButtonText ]
  ]

let createAddNewItemControl dispatch child item =

  let updateCurrent s =
    (child, s) |> UpdatingItem |> dispatch

  let onBlur =
    (Props.OnBlur (fun _ -> dispatch EndedUpdatingItem))

  createInputButtonCombo
    { Dispatch = dispatch
      InputValue = item
      ButtonText = "add item"
      PlaceholderText = "Enter item name"
      OnChange = updateCurrent
      OnCommitMsg = AddedItem
      AdditionalInputProps = [ onBlur ] }

let renderNiceChild dispatch currentItem childName items =

  let itemList =
    items
    |> List.map (fun item -> li [] [ str item.Description ])

  let addControl =
    match currentItem with
    | Some (cName, item) when cName = childName -> createAddNewItemControl dispatch childName item
    | _ -> li [ ] [ createStartAddButton dispatch childName ]

  tr [ ] [
    td [ ColSpan 3 ] [
      strong [ ] [ str childName ]
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

let renderUndecidedChild dispatch childName =

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
        Button.OnClick (fun _ -> dispatch (ReviewedChild (childName, non)))
      ] [ str (text) ]
    ]

  tr [ ] [
    td [ Props.HTMLAttr.Width "100%" ] [ str childName ]
    td [ ] [ createButton (Nice []) ]
    td [ ] [ createButton Naughty ]
  ]

let renderChildListItem dispatch currentItem child =

  let content =
    match child.NaughtyOrNice with
    | Nice items ->
      renderNiceChild dispatch currentItem child.Name items
    | Undecided ->
      renderUndecidedChild dispatch child.Name
    | Naughty ->
      renderNaughtyChild child.Name

  content

let renderChildList dispatch model =

  let childList =
    model.ChildrensList
    |> List.map (fun c -> renderChildListItem dispatch model.CurrentEditor.CurrentItem c)

  Content.content [ ] [
    if not (List.isEmpty model.ChildrensList) then
      yield Heading.h3 [ ] [ str "List of children" ]

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
        { Dispatch = dispatch
          InputValue = model.CurrentEditor.EditingChildName
          ButtonText = "add"
          PlaceholderText = "Enter child's name"
          OnChange = updatingCurrent
          OnCommitMsg = AddedChild
          AdditionalInputProps = [] }
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
      Heading.h3 [ ] [ str "Santa's list" ]
      ul [ ] items
    ]

let root model dispatch =
  div [ ] [

    Navbar.navbar [ Navbar.Color Color.IsPrimary ] [
      Navbar.Brand.div [ ] [
         a [ Href "/" ] [
          Image.image [ Image.Is48x48 ] [
            img [ Src "img/brand.png" ]
          ]]]

      Navbar.Item.div [ ] [
        Heading.h2 [
        ] [ str "Santa's Xmas Manager"]
      ]
    ]

    Container.container [ ] [
      renderAddChild dispatch model
      renderChildList dispatch model
      renderSantasList model.SantasList
    ]
  ]
