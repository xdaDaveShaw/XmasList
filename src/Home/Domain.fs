module Home.Domain

open Home.Types

type AddChild = Model -> UnknownChild -> Model
type AddItem = Model -> NiceChild -> Item -> Model
type ReviewChild = Model -> UnknownChild -> NaughtyOrNice -> Model

// Private functions for aggreates
type private AddItemToChild = NiceChild -> Item -> NiceChild
type private UpdateChild = Child list -> Child -> Child list
type private UpdateSantasList = SantasItem list -> Item -> SantasItem list

let private canAdd newItem items =
  not (System.String.IsNullOrEmpty(newItem.Description))
  &&
  items
  |> List.tryFind (fun i -> i.Description = newItem.Description)
  |> Option.isNone

let addChild : AddChild =
  fun model child ->
    let newChildren = model.ChildrensList @ [ Unknown child ]
    { model with ChildrensList = newChildren }

let private addItemToChild : AddItemToChild =
  fun child item ->
    if canAdd item child.Items then
      { child with Items = child.Items @ [ item ] }
    else
      child

let private updateChild : UpdateChild =
  fun xs child ->

    let name =
      match child with
      | Nice c -> c.Name
      | Naughty c -> c.Name
      | Unknown c -> c.Name

    let update c =
      match c with
      | Nice c -> if c.Name = name then child else Nice c
      | Naughty c -> if c.Name = name then child else Naughty c
      | x -> x

    xs
    |> List.map update

let private addItemToSantasList : UpdateSantasList =
  fun xs item ->

    let existingItem =
      xs
      |> List.tryFind (fun i -> i.Name = item.Description)

    match existingItem with
    | None -> xs @ [ { Name = item.Description; Quantity = 1 } ]
    | Some existing ->
      let update i =
        if i.Name = existing.Name then
          { i with Quantity = i.Quantity + 1 }
        else
          i
      xs
      |> List.map update

let addItem : AddItem =
  fun model child item ->
    let newChild = addItemToChild child item
    let newChildList = updateChild model.ChildrensList (Nice newChild)
    let newSantaList = addItemToSantasList model.SantasList item

    { model with
        ChildrensList = newChildList
        SantasList = newSantaList }

let reviewChild : ReviewChild =
  fun model child non ->
    let name = child.Name

    let newChild =
      match non with
      | NaughtyOrNice.Nice -> Nice { Name = name; Items = [] }
      | NaughtyOrNice.Naughty -> Naughty { Name = name; }

    let newChildList =
      updateChild model.ChildrensList newChild

    { model with ChildrensList = newChildList }
