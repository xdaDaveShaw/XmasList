module XmasList.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { ChildrensList = []
    CurrentEditor = { EditingChildName = ""; CurrentItem = None }
    SantasList = [] },
  []

let updateEditorState model newState =
  { model with CurrentEditor = newState }

let updatingCurrentChild model name =
  let newState = { model.CurrentEditor with EditingChildName = name }
  updateEditorState model newState

let updatingCurrentItem model child item =
  let newState = { model.CurrentEditor with CurrentItem = Some (child, item) }
  updateEditorState model newState

let endedUpdatingItem model =
  let newState = { model.CurrentEditor with CurrentItem = None }
  updateEditorState model newState

let clearCurrentChild model =
  let newState = { model.CurrentEditor with EditingChildName = "" }
  updateEditorState model newState

let addedChild model =
  Domain.addChild model.CurrentEditor.EditingChildName model
  |> clearCurrentChild

let clearCurrentItem model =
  let newItem =
    model.CurrentEditor.CurrentItem
    |> Option.map (fun (child, _) -> child, "")

  let newState = { model.CurrentEditor with CurrentItem = newItem }
  updateEditorState model newState

let addedItem model =
  let newModel =
    match model.CurrentEditor.CurrentItem with
    | Some (child, item) -> Domain.addItem child { Description = item } model
    | _ -> model
  clearCurrentItem newModel

let reviewedChild child naughtyOrNice model =
  Domain.reviewChild child naughtyOrNice model

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdatingChild name ->
    updatingCurrentChild model name, []
  | UpdatingItem (child, item) ->
    updatingCurrentItem model child item, []
  | EndedUpdatingItem ->
    endedUpdatingItem model, []
  | AddedChild ->
    addedChild model, []
  | AddedItem ->
    addedItem model, []
  | ReviewedChild (child, naughtyOrNice) ->
    reviewedChild child naughtyOrNice model, []
