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

let clearCurrent model =
  let newCurrent =
    match model.CurrentEntry with
    | Child _ -> Child ""
    | Item (c, _) -> Item (c, "")

  { model with CurrentEntry = newCurrent }

let addedChild model =
  let newModel =
    match model.CurrentEntry with
    | CurrentEntry.Child name -> Domain.addChild model { Name = name; }
    | _ -> model
  clearCurrent newModel

let addedItem model =
  let newModel =
    match model.CurrentEntry with
    | CurrentEntry.Item (child, item) -> Domain.addItem model child { Description = item }
    | _ -> model
  clearCurrent newModel

let reviewedChild model child naughtyOrNice =
  Domain.reviewChild model child naughtyOrNice

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
