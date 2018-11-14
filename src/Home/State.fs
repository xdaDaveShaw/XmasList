module Home.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { Name = ""
    Current = { Id = 0; Description = ""}
    Items = []
    State = Unknown },
  []

let canAdd current items =
  not (System.String.IsNullOrEmpty(current.Description))
  &&
  items
  |> List.tryFind (fun i -> i.Description = current.Description)
  |> Option.isNone

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdatingName str ->
    { model with Name = str }, []

  | AcceptedName ->
    { model with
        State = Unsent
        Items = [ ] }, []

  | UpdatingCurrent text ->
    { model with Current = { model.Current with Description = text } }, []

  | AddCurrentToList ->
    if canAdd model.Current model.Items then
      { model with
          Items = model.Items @ [ model.Current ]
          Current = { Id = model.Current.Id + 1; Description = "" } }, []
    else
      model, []

  | Sending ->
    { model with State = ListState.Sending }, []

  | CancelledSend ->
    { model with State = ListState.Unsent }, []

  | Sent ->
    { model with State = ListState.Sent }, []
