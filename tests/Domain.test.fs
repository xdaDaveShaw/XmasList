module XmasList.Tests

open Fable.Import.Jest
open Fable.Import.Jest.Matchers
open Types

let defaultModel =
  fst (XmasList.State.init())

test "Adding children works" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Undecided }
  let child2 = { Name = "Shaw"; NaughtyOrNice = Undecided }
  let newModel = Domain.addChild defaultModel child1
  let newModel = Domain.addChild newModel child2
  let addedChild = newModel.ChildrensList
  [ child1; child2 ] == addedChild

test "Cannot add child with no name" <| fun () ->
  let child = { Name = ""; NaughtyOrNice = Undecided }
  let newModel = Domain.addChild defaultModel child

  defaultModel == newModel

test "Cannot add child twice" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Undecided }
  let child2 = { Name = "Dave"; NaughtyOrNice = Undecided }

  let modelAfter1 = Domain.addChild defaultModel child1
  let modelAfter2 = Domain.addChild modelAfter1 child2

  modelAfter1 == modelAfter2

test "Reviewing a nice child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }

  let newModel = Domain.reviewChild model child (Nice [])

  let expected = { child with NaughtyOrNice = Nice [] }
  let actual = newModel.ChildrensList |> List.head
  expected == actual

test "Reviewing a naughty child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }

  let newModel = Domain.reviewChild model child Naughty

  let expected = { child with NaughtyOrNice = Naughty }
  let actual = newModel.ChildrensList |> List.head
  expected == actual

test "Cannot add item to naughty child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Naughty }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem model child item

  model == newModel

test "Cannot add item to undecided child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Undecided }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem model child item

  model == newModel

test "Adding first item to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem model child item

  let expectedChild = { child with NaughtyOrNice = Nice [ item ] }
  let actualChild = newModel.ChildrensList |> List.head
  let expectedSanta = { ItemName = "Book"; Quantity = 1 }
  let actualSanta = newModel.SantasList
  expectedChild == actualChild
  [ expectedSanta ] == actualSanta

test "Cannot add item with no name to a child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "" }

  let newModel = Domain.addItem model child item

  let expectedChild = { child with NaughtyOrNice = Nice [] }
  let actualChild = newModel.ChildrensList |> List.head
  let actualSanta = newModel.SantasList
  expectedChild == actualChild
  [] == actualSanta


test "Cannot add duplicate item to same child" <| fun () ->
  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem model child item
  let newChild = newModel.ChildrensList |> List.head
  let newModel = Domain.addItem newModel newChild item

  let expectedChild = { child with NaughtyOrNice = Nice [ item ] }
  let actualChildren = newModel.ChildrensList
  let expectedSanta = { ItemName = "Book"; Quantity = 1 }
  let actualSanta = newModel.SantasList
  [ expectedChild ] == actualChildren
  [ expectedSanta ] == actualSanta

test "Can add duplicate item to different child" <| fun () ->
  let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child1; child2 ] }
  let item = { Description = "Book" }

  let newModel = Domain.addItem model child1 item
  let newModel = Domain.addItem newModel child2 item

  let expectedChildren = [
    { child1 with NaughtyOrNice = Nice [ item ] }
    { child2 with NaughtyOrNice = Nice [ item ] }
  ]
  let actualChildren = newModel.ChildrensList
  let expectedSanta = { ItemName = "Book"; Quantity = 2 }
  let actualSanta = newModel.SantasList
  expectedChildren == actualChildren
  [ expectedSanta ] == actualSanta


test "Adding subsequent items to a child" <| fun () ->

  let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
  let model = { defaultModel with ChildrensList = [ child ] }

  let item1 = { Description = "Book" }
  let item2 = { Description = "Hat" }
  let item3 = { Description = "Scarf" }

  let newModel = Domain.addItem model child item1
  let newChild = newModel.ChildrensList |> List.head
  let newModel = Domain.addItem newModel newChild item2
  let newChild = newModel.ChildrensList |> List.head
  let newModel = Domain.addItem newModel newChild item3

  let expectedChild = { child with NaughtyOrNice = Nice [ item1; item2; item3; ] }
  let actualChild = newModel.ChildrensList |> List.head
  let expectedSanta = [
    { ItemName = "Book"; Quantity = 1 }
    { ItemName = "Hat"; Quantity = 1 }
    { ItemName = "Scarf"; Quantity = 1 }
  ]
  let actualSanta = newModel.SantasList
  expectedChild == actualChild
  expectedSanta == actualSanta
