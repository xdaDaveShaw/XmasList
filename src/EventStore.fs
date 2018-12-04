module XmasList.EventStore

open Fable.Import

type Name = string
type Item = string
type Review = string

type Event =
  | AddedChild of Name
  | AddedItem of Name * Item
  | ReviewedChild of Name * Review

open Thoth.Json

let private getEventNameAndData = function
  | AddedChild name ->
    "addedChild", Encode.object
      [ "name", Encode.string name ]
  | AddedItem (childName, itemName) ->
    "addedItem", Encode.object
      [ "childName", Encode.string childName
        "itemName", Encode.string itemName ]
  | ReviewedChild (childName, review) ->
    "reviewedChild", Encode.object
      [ "childName", Encode.string childName
        "review", Encode.string review ]

let private encodeEvent event =
  let eventName, data = getEventNameAndData event

  Encode.object
    [ "eventName", Encode.string eventName
      "data", data ]

let private backingStore = new ResizeArray<Event>()

let storeEvent event =
  backingStore.Add(event)

  let encodedEvents =
      backingStore
      |> Seq.map encodeEvent
      |> Seq.toList

  let encodedEventList = Encode.list encodedEvents

  let json = Encode.toString 0 encodedEventList

  Browser.localStorage.setItem("xmas-list", json)
