module XmasList.Tests

open Fable.Import.Jest
open Fable.Import.Jest.Matchers
open Types

let defaultModel =
  fst (XmasList.State.init())

test "Adding children works" <| fun () ->
  let child1 = "Dave"
  let child2 = "Shaw"

  let newModel =
    Domain.addChild child1 defaultModel
    |> Domain.addChild child2

  let expected = [
    { Name = child1; NaughtyOrNice = Undecided }
    { Name = child2; NaughtyOrNice = Undecided }
  ]

  newModel.ChildrensList == expected


test "Cannot add child with no name" <| fun () ->
  let child = ""
  let newModel = Domain.addChild child defaultModel

  defaultModel == newModel

test "Cannot add child twice" <| fun () ->
  let modelAfter1 = Domain.addChild "Dave" defaultModel
  let modelAfter2 = Domain.addChild "Dave" modelAfter1

  modelAfter1 == modelAfter2

test "Reviewing a nice child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }

  let newModel = Domain.reviewChild "Dave" (Nice []) model

  let expected = { child with NaughtyOrNice = Nice [] }
  let actual = newModel.ChildrensList |> List.head
  actual == expected

test "Reviewing a naughty child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }

  let newModel = Domain.reviewChild "Dave" Naughty model

  let expected = { child with NaughtyOrNice = Naughty }
  let actual = newModel.ChildrensList |> List.head
  actual == expected

test "Cannot add item to naughty child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Naughty }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem "Dave" item model

  model == newModel

test "Cannot add item to undecided child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem "Dave" item model

  model == newModel

test "Adding first item to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem "Dave" item model

  let expectedChild = [ { child with NaughtyOrNice = Nice [ item ] } ]
  let actualChild = newModel.ChildrensList
  let expectedSanta = { ItemName = "Book"; Quantity = 1 }
  let actualSanta = newModel.SantasList
  actualChild == expectedChild
  actualSanta == [ expectedSanta ]

test "Cannot add item with no name to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "" }

  let newModel = Domain.addItem "Dave" item model

  let expectedChild = [ { child with NaughtyOrNice = Nice [] } ]
  let actualChild = newModel.ChildrensList
  let actualSanta = newModel.SantasList
  actualChild == expectedChild
  actualSanta == []


test "Cannot add duplicate item to same child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel =
    Domain.addItem "Dave" item model
    |> Domain.addItem "Dave" item

  let expectedChild = [ { child with NaughtyOrNice = Nice [ item ] } ]
  let actualChildren = newModel.ChildrensList
  let expectedSanta = [ { ItemName = "Book"; Quantity = 1 } ]
  let actualSanta = newModel.SantasList
  actualChildren == expectedChild
  actualSanta == expectedSanta

test "Can add duplicate item to different child" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child1; child2 ] }
  let item = { Description = "Book" }

  let newModel =
    Domain.addItem "Dave" item model
    |> Domain.addItem "Shaw" item

  let expectedChildren = [
    { child1 with NaughtyOrNice = Nice [ item ] }
    { child2 with NaughtyOrNice = Nice [ item ] }
  ]
  let actualChildren = newModel.ChildrensList
  let expectedSanta = [ { ItemName = "Book"; Quantity = 2 } ]
  let actualSanta = newModel.SantasList
  actualChildren == expectedChildren
  actualSanta == expectedSanta


test "Adding subsequent items to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }

  let item1 = { Description = "Book" }
  let item2 = { Description = "Hat" }
  let item3 = { Description = "Scarf" }

  let newModel =
    Domain.addItem "Dave" item1 model
    |> Domain.addItem "Dave" item2
    |> Domain.addItem "Dave" item3

  let expectedChild = { child with NaughtyOrNice = Nice [ item1; item2; item3; ] }
  let actualChild = newModel.ChildrensList |> List.head
  let expectedSanta = [
    { ItemName = "Book"; Quantity = 1 }
    { ItemName = "Hat"; Quantity = 1 }
    { ItemName = "Scarf"; Quantity = 1 }
  ]
  let actualSanta = newModel.SantasList
  actualChild == expectedChild
  actualSanta == expectedSanta


test "Adding duplicate casing of items items to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }

  let item1 = { Description = "Book" }
  let item2 = { Description = "book" }

  let newModel =
    Domain.addItem "Dave" item1 model
    |> Domain.addItem "Dave" item2

  let expectedChild = { child with NaughtyOrNice = Nice [ item1; ] }
  let actualChild = newModel.ChildrensList |> List.head
  let expectedSanta = [
    { ItemName = "Book"; Quantity = 1 }
  ]
  let actualSanta = newModel.SantasList
  actualChild == expectedChild
  actualSanta == expectedSanta

test "Adding duplicate casing of items items to different children" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child1; child2 ] }
  let item1 = { Description = "Book" }
  let item2 = { Description = "book" }

  let newModel =
    Domain.addItem "Dave" item1 model
    |> Domain.addItem "Shaw" item2

  let expectedChildren = [
    { child1 with NaughtyOrNice = Nice [ item1 ] }
    { child2 with NaughtyOrNice = Nice [ item2 ] }
  ]
  let actualChildren = newModel.ChildrensList
  let expectedSanta = [ { ItemName = "Book"; Quantity = 2 } ]
  let actualSanta = newModel.SantasList
  actualChildren == expectedChildren
  actualSanta == expectedSanta

test "Ensure leading/trailing whitespace is removed on items" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child1; child2 ] }
  let item1 = { Description = " book" }
  let item2 = { Description = "book " }

  let newModel =
    Domain.addItem "Dave" item1 model
    |> Domain.addItem "Shaw" item2

  let expectedItem = { Description = "book" }

  let expectedChildren = [
    { child1 with NaughtyOrNice = Nice [ expectedItem ] }
    { child2 with NaughtyOrNice = Nice [ expectedItem ] }
  ]
  let actualChildren = newModel.ChildrensList
  let expectedSanta = [ { ItemName = expectedItem.Description; Quantity = 2 } ]
  let actualSanta = newModel.SantasList
  actualChildren == expectedChildren
  actualSanta == expectedSanta

test "Ensure leading/trailing whitespace is removed on items" <| fun () ->
  let modelAfter1 = Domain.addChild " Dave" defaultModel
  let modelAfter2 = Domain.addChild "Dave " modelAfter1

  let expectedChildren = [ { Name = "Dave"; NaughtyOrNice = Undecided } ]

  modelAfter1 == modelAfter2
  modelAfter2.ChildrensList == expectedChildren
