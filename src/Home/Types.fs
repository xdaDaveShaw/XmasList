module Home.Types

type Item = {
  Id: int
  Description: string
}

type Model = {
  Name: string
  IsAccepted: bool
  Current: Item
  Items: Item list
}

type Msg =
  | UpdatingName of string
  | AcceptName
  | UpdatingCurrent of string
  | Add
