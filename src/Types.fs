module XmasList.Types

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

type CurrentEditorState = {
  EditingChildName: string
  CurrentItem: (string * string) option
}

type Model = {
  CurrentEditor: CurrentEditorState
  ChildrensList: Child list
  SantasList: SantasItem list
}

type Msg =
  | UpdatingChild of string
  | UpdatingItem of string * string
  | EndedUpdatingItem
  | AddedChild
  | AddedItem
  | ReviewedChild of string * NaughtyOrNice
