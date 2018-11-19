module Home.Types

type Item = {
  Id: int
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

type Child =
  | Unknown of UnknownChild
  | Nice of NiceChild
  | Naughty of NaughtyChild

type SantasItem = {
  Name: string
  Quantity: int
}

type CurrentEntry =
  | Child of string
  | Item of Item

type Model = {
  CurrentEntry: CurrentEntry
  ChildrensList: Child list
  SantasList: SantasItem list
}

type Msg =
  | UpdatingCurrent of CurrentEntry
  | AddedChild
  | AddedItem of NiceChild
  | ReviewedChild of UnknownChild * NaughtyOrNice
