module XmasList.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =

  let events = EventStore.loadEvents()
  let model = Domain.fromEvents events

  model, []

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
  let newModel, event =
    Domain.addChild model.CurrentEditor.EditingChildName model

  EventStore.storeEvent event

  newModel |> clearCurrentChild

let clearCurrentItem model =
  let newItem =
    model.CurrentEditor.CurrentItem
    |> Option.map (fun (child, _) -> child, "")

  let newState = { model.CurrentEditor with CurrentItem = newItem }
  updateEditorState model newState

let addedItem model =

  let newModel, msg =
    match model.CurrentEditor.CurrentItem with
    | Some (_, item) when item = "" -> model, Cmd.ofMsg EndedUpdatingItem
    | Some (child, item) ->
      let newModel, event = Domain.addItem child { Description = item } model
      EventStore.storeEvent event
      newModel, Cmd.none
    | _ -> model, Cmd.none

  newModel |> clearCurrentItem, msg

let reviewedChild child naughtyOrNice model =
  let newModel, event = Domain.reviewChild child naughtyOrNice model

  EventStore.storeEvent event

  let msg =
    match naughtyOrNice with
    | Nice _ -> Cmd.ofMsg (UpdatingItem (child, ""))
    | _ -> Cmd.none

  newModel, msg

let updateClearingStorage clearing model =
  let newEditor = { model.CurrentEditor with ClearingStorage = clearing }
  { model with CurrentEditor = newEditor }

let beginClearStorage model =
  model |> updateClearingStorage true

let endClearStorage model =
  model |> updateClearingStorage false

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
    addedItem model
  | ReviewedChild (child, naughtyOrNice) ->
    reviewedChild child naughtyOrNice model
  | BeginClearStorage ->
    beginClearStorage model, []
  | PerformClearStorage ->
    EventStore.clearStorage()
    Domain.defaultModel, Cmd.ofMsg EndClearStorage
  | EndClearStorage ->
    endClearStorage model, []
