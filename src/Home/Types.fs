module Home.Types

type Model = {
  Name: string
  Accepted: bool }

type Msg =
  | ChangeStr of string
  | AcceptName
