module Home.Types

type Item = {
  Id: int
  Description: string
}

type ListState =
  | Unknown
  | Unsent
  | Sending
  | Sent

type Model = {
  Name: string
  Current: Item
  Items: Item list
  State: ListState
}

type Msg =
  | UpdatingName of string
  | AcceptedName
  | UpdatingCurrent of string
  | AddCurrentToList
  | Sending
  | Sent
  | CancelledSend
