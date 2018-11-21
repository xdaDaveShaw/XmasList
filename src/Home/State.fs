module Home.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { ChildrensList = []
    CurrentEntry = Child ""
    SantasList = [] },
  []

let updatingCurrent model entry =
  { model with CurrentEntry = entry }

let clearCurrentChild model =
  { model with CurrentEntry = Child "" }

let clearCurrentItem model =
  match model.CurrentEntry with
  | Child _ -> model
  | Item (c, _) -> { model with CurrentEntry = Item (c, "") }

let addedChild model =
  let newModel =
    match model.CurrentEntry with
    | CurrentEntry.Child name -> Domain.addChild model { Name = name; NaughtyOrNice = Undecided }
    | _ -> model
  clearCurrentChild newModel

let addedItem model =
  let newModel =
    match model.CurrentEntry with
    | CurrentEntry.Item (child, item) -> Domain.addItem model child { Description = item }
    | _ -> model
  clearCurrentItem newModel

let reviewedChild model child naughtyOrNice =
  Domain.reviewChild model child naughtyOrNice
  |> clearCurrentChild

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdatingCurrent entry ->
    updatingCurrent model entry, []
  | AddedChild ->
    addedChild model, []
  | AddedItem ->
    addedItem model, []
  | ReviewedChild (child, naughtyOrNice) ->
    reviewedChild model child naughtyOrNice, []
