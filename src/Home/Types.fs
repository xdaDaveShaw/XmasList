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
  IsSending: bool
  IsSent: bool
}

type Msg =
  | UpdatingName of string
  | AcceptName
  | UpdatingCurrent of string
  | Add
  | Sending
  | Send
  | CancelSend
