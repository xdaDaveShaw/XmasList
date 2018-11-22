module XmasList.Domain

open XmasList.Types

type AddChild = Model -> Child -> Model
type AddItem = Model -> Child -> Item -> Model
type ReviewChild = Model -> Child -> NaughtyOrNice -> Model

// Private functions for aggreates
type private AddItemToChild = Child -> Item -> Child * bool
type private UpdateChild = Child list -> Child -> Child list
type private UpdateSantasList = SantasItem list -> Item -> SantasItem list

let private canAddItem newItem items =
  not (System.String.IsNullOrEmpty(newItem.Description))
  &&
  items
  |> List.tryFind (fun i -> i.Description = newItem.Description)
  |> Option.isNone

let private canAddChild (newChild : Child) (children : Child list) =
  not (System.String.IsNullOrEmpty(newChild.Name))
  &&
  children
  |> List.tryFind (fun child -> child.Name = newChild.Name)
  |> Option.isNone

let addChild : AddChild =
  fun model child ->
    if canAddChild child model.ChildrensList then
      { model with ChildrensList = model.ChildrensList @ [ child ] }
    else
      model

let private addItemToChild : AddItemToChild =
  fun child item ->
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
  fun xs item ->

    let existingItem =
      xs
      |> List.tryFind (fun i -> i.ItemName = item.Description)

    match existingItem with
    | None -> xs @ [ { ItemName = item.Description; Quantity = 1 } ]
    | Some existing ->
      let update i =
        if i.ItemName = existing.ItemName then
          { i with Quantity = i.Quantity + 1 }
        else
          i
      xs
      |> List.map update

let addItem : AddItem =
  fun model child item ->

    let newChild, success = addItemToChild child item

    let newChildList, newSantaList =
      if success then
        let ncl = updateChild model.ChildrensList newChild
        let nsl = addItemToSantasList model.SantasList item

        ncl, nsl
      else
        model.ChildrensList, model.SantasList

    { model with
        ChildrensList = newChildList
        SantasList = newSantaList }

let reviewChild : ReviewChild =
  fun model child non ->

    let newChild =
      { child with NaughtyOrNice = non}

    let newChildList =
      updateChild model.ChildrensList newChild

    { model with ChildrensList = newChildList }