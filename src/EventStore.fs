module XmasList.EventStore

open Fable.Core.JsInterop
open Fable.Import

type Name = string
type Item = string
type Review = string

type Event =
  | AddedChild of Name
  | AddedItem of Name * Item
  | ReviewedChild of Name * Review

open Thoth.Json

let private backingStore = new ResizeArray<Event>()

[<Literal>]
let Key = "xmas-list"

let storeEvent event =
  backingStore.Add(event)

  let json = Encode.Auto.toString(0, backingStore)

  Browser.WebStorage.localStorage.setItem(Key, json)

let decode json =
  match Decode.Auto.fromString<Event list>(json) with
  | Ok events -> events
  | Error _ -> []

let loadEvents() =
  let events =
    !!Browser.WebStorage.localStorage.getItem(Key)
    |> Option.map decode
    |> Option.defaultValue []
  backingStore.AddRange(events)
  events

let clearStorage() =
  Browser.WebStorage.localStorage.removeItem Key
