module Home.Types

type Item = {
  Description: string
}

type NaughtyOrNice =
  | Undecided
  | Nice of Item list
  | Naughty

type Child = {
  Name: string
  NaughtyOrNice: NaughtyOrNice
}

type SantasItem = {
  ItemName: string
  Quantity: int
}

type CurrentEntry =
  | Nothing
  | Child of string
  | Item of Child * string

type Model = {
  CurrentEntry: CurrentEntry
  ChildrensList: Child list
  SantasList: SantasItem list
}

type Msg =
  | UpdatingCurrent of CurrentEntry
  | AddedChild
  | AddedItem
  | ReviewedChild of Child * NaughtyOrNice
