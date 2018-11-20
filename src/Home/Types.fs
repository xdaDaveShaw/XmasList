module Home.Types

type Item = {
  Description: string
}

type NaughtyOrNice =
  | Nice
  | Naughty

type UnknownChild = {
  Name: string
}

type NiceChild = {
  Name: string
  Items: Item list
}

type NaughtyChild = {
  Name: string
}

//TODO: Name + NaughtyOrNice
type Child =
  | Unknown of UnknownChild
  | Nice of NiceChild
  | Naughty of NaughtyChild

let getChildName = function
  | Nice c -> c.Name
  | Naughty c -> c.Name
  | Unknown c -> c.Name

type SantasItem = {
  Name: string
  Quantity: int
}

type CurrentEntry =
  | Child of string
  | Item of NiceChild * string

type Model = {
  CurrentEntry: CurrentEntry
  ChildrensList: Child list
  SantasList: SantasItem list
}

type Msg =
  | UpdatingCurrent of CurrentEntry
  | AddedChild
  | AddedItem
  | ReviewedChild of UnknownChild * NaughtyOrNice
