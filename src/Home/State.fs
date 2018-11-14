module Home.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { Name = ""
    Current = { Id = 0; Description = ""}
    Items = []
    State = Unknown },
  []

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
    if (model.Current.Description.Length > 0) then
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
