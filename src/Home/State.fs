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

let addedChild model =
  match model.CurrentEntry with
  | CurrentEntry.Child c -> Domain.addChild model { Name = c; }
  | CurrentEntry.Item i -> model

let addedItem model child =
  match model.CurrentEntry with
  | CurrentEntry.Child _ -> model
  | CurrentEntry.Item i -> Domain.addItem model child i

let reviewedChild model child naughtyOrNice =
  Domain.reviewChild model child naughtyOrNice

let update2 msg model : Model * Cmd<Msg> =
  match msg with
  | UpdatingCurrent entry ->
    updatingCurrent model entry, []
  | AddedChild ->
    addedChild model, []
  | AddedItem child ->
    addedItem model child, []
  | ReviewedChild (child, naughtyOrNice) ->
    reviewedChild model child naughtyOrNice, []
