module XmasList.Domain

open XmasList.Types

type AddChild = string -> Model -> Model * EventStore.Event
type AddItem = string -> Item -> Model -> Model * EventStore.Event
type ReviewChild = string -> NaughtyOrNice -> Model -> Model * EventStore.Event
type FromEvents = CurrentEditorState -> EventStore.Event list -> Model

// Private functions for aggreates
type private AddItemToChild = Child -> Item -> Child * bool
type private UpdateChild = Child list -> Child -> Child list
type private UpdateSantasList = SantasItem list -> Item -> SantasItem list

let createDefaultModel editorState =
  { ChildrensList = []
    CurrentEditor = editorState
    SantasList = [] }

let private equalCI a b =
  System.String.Equals(a, b, System.StringComparison.CurrentCultureIgnoreCase)

let private canAddChild name children =
  not (System.String.IsNullOrEmpty(name))
  &&
  children
  |> List.tryFind (fun child -> child.Name = name)
  |> Option.isNone

let addChild : AddChild =
  fun child model ->

    let event = EventStore.AddedChild child

    let child = child.Trim()

    let newModel =
      if canAddChild child model.ChildrensList then
        { model with ChildrensList = model.ChildrensList @ [ { Name = child; NaughtyOrNice = Undecided; } ] }
      else
        model

    newModel, event

let private canAddItem newItem items =
  not (System.String.IsNullOrEmpty(newItem.Description))
  &&
  items
  |> List.tryFind (fun i -> equalCI i.Description newItem.Description)
  |> Option.isNone

let private sanatiseItem item =
  { item with Description = item.Description.Trim() }

let private addItemToChild : AddItemToChild =
  fun child item ->

    let item = sanatiseItem item

    match child.NaughtyOrNice with
    | Nice items when canAddItem item items ->
      { child with NaughtyOrNice = Nice (items @ [ item ]) }, true
    | _ ->
      child, false

let private updateChild : UpdateChild =
  fun xs newChild ->

    let update c =
      if c.Name = newChild.Name then newChild else c

    xs
    |> List.map update

let private addItemToSantasList : UpdateSantasList =
  fun santasItems item ->

    let item = sanatiseItem item

    let existingItem =
      santasItems
      |> List.tryFind (fun i -> equalCI i.ItemName item.Description)

    match existingItem with
    | None -> santasItems @ [ { ItemName = item.Description; Quantity = 1 } ]
    | Some existing ->
      let update i =
        if i = existing then
          { i with Quantity = i.Quantity + 1 }
        else
          i
      santasItems
      |> List.map update

let private findExistingChild model name =
  model.ChildrensList
  |> List.find (fun c -> c.Name = name)

let addItem : AddItem =
  fun child item model ->

    let event = EventStore.AddedItem (child, item.Description)

    let existingChild = findExistingChild model child

    let newChild, success = addItemToChild existingChild item

    let newChildList, newSantaList =
      if success then
        let ncl = updateChild model.ChildrensList newChild
        let nsl = addItemToSantasList model.SantasList item

        ncl, nsl
      else
        model.ChildrensList, model.SantasList

    { model with
        ChildrensList = newChildList
        SantasList = newSantaList }, event

let private nonToString = function
  | Undecided -> "Undecided"
  | Nice _ -> "Nice"
  | Naughty -> "Naughty"

let private stringToNon = function
  | "Undecided" -> Undecided
  | "Nice" -> Nice []
  | "Naughty" -> Naughty
  | s -> failwith (sprintf "Unknown Naughty Or Nice: %s" s)

let reviewChild : ReviewChild =
  fun child non model ->

    let event = EventStore.ReviewedChild (child, non |> nonToString)

    let existingChild = findExistingChild model child

    let newChild =
      { existingChild with NaughtyOrNice = non}

    let newChildList =
      updateChild model.ChildrensList newChild

    { model with ChildrensList = newChildList }, event

let fromEvents : FromEvents =
  fun editorState events ->

    let processEvent m ev =
      let model, _ =
        match ev with
        | EventStore.AddedChild name -> m |> addChild name
        | EventStore.ReviewedChild (name, non) -> m |> reviewChild name (stringToNon non)
        | EventStore.AddedItem (name, item) -> m |> addItem name { Description = item }
      model

    let model =
      createDefaultModel editorState

    (model, events)
    ||> List.fold processEvent

