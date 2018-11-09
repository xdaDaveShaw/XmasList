module Home.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { Name = ""
    IsAccepted = false
    Current = { Id = 0; Description = ""}
    Items = [] },
  []

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdatingName str ->
    { model with Name = str }, []
  | AcceptName ->
    { model with
        IsAccepted = true
        Items = [ ] }, []
  | UpdatingCurrent text ->
    { model with Current = { model.Current with Description = text } }, []
  | Add ->
    { model with
        Items = model.Items @ [ model.Current ]
        Current = { Id = model.Current.Id + 1; Description = "" } }, []
  | Send -> model, []
