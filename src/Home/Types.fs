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
  | AcceptName
  | UpdatingCurrent of string
  | Add
  | Sending
  | Send
  | CancelSend
