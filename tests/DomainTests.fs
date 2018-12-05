module DomainTests

open XmasList
open XmasList.Types
open Util

let private defaultModel =
  fst (State.init())

let addItem n i m =
  let r, _ = Domain.addItem n i m
  r

let addChild n m =
  let r, _ = Domain.addChild n m
  r

let reviewChild n non m =
  let r, _ = Domain.reviewChild n non m
  r

let testCases =
  [
    testCase "Adding children works" <| fun () ->
      let child1 = "Dave"
      let child2 = "Shaw"

      let newModel =
        addChild child1 defaultModel
        |> addChild child2

      let expected = [
        { Name = child1; NaughtyOrNice = Undecided }
        { Name = child2; NaughtyOrNice = Undecided } ]

      newModel.ChildrensList == expected


    testCase "Cannot add child with no name" <| fun () ->
      let child = ""
      let newModel = addChild child defaultModel

      defaultModel == newModel

    testCase "Cannot add child twice" <| fun () ->
      let modelAfter1 = addChild "Dave" defaultModel
      let modelAfter2 = addChild "Dave" modelAfter1

      modelAfter1 == modelAfter2

    testCase "Reviewing a nice child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Undecided }
      let model = { defaultModel with ChildrensList = [ child ] }

      let newModel = reviewChild "Dave" (Nice []) model

      let expected = { child with NaughtyOrNice = Nice [] }
      let actual = newModel.ChildrensList |> List.head
      actual == expected

    testCase "Reviewing a naughty child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Undecided }
      let model = { defaultModel with ChildrensList = [ child ] }

      let newModel = reviewChild "Dave" Naughty model

      let expected = { child with NaughtyOrNice = Naughty }
      let actual = newModel.ChildrensList |> List.head
      actual == expected

    testCase "Cannot add item to naughty child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Naughty }
      let model = { defaultModel with ChildrensList = [ child ] }
      let item = { Description = "Book" }

      let newModel = addItem "Dave" item model

      model == newModel

    testCase "Cannot add item to undecided child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Undecided }
      let model = { defaultModel with ChildrensList = [ child ] }
      let item = { Description = "Book" }

      let newModel = addItem "Dave" item model

      model == newModel

    testCase "Adding first item to a child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child ] }
      let item = { Description = "Book" }

      let newModel = addItem "Dave" item model

      let expectedChild = [ { child with NaughtyOrNice = Nice [ item ] } ]
      let actualChild = newModel.ChildrensList
      let expectedSanta = { ItemName = "Book"; Quantity = 1 }
      let actualSanta = newModel.SantasList
      actualChild == expectedChild
      actualSanta == [ expectedSanta ]

    testCase "Cannot add item with no name to a child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child ] }
      let item = { Description = "" }

      let newModel = addItem "Dave" item model

      let expectedChild = [ { child with NaughtyOrNice = Nice [] } ]
      let actualChild = newModel.ChildrensList
      let actualSanta = newModel.SantasList
      actualChild == expectedChild
      actualSanta == []


    testCase "Cannot add duplicate item to same child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child ] }
      let item = { Description = "Book" }

      let newModel =
        addItem "Dave" item model
        |> addItem "Dave" item

      let expectedChild = [ { child with NaughtyOrNice = Nice [ item ] } ]
      let actualChildren = newModel.ChildrensList
      let expectedSanta = [ { ItemName = "Book"; Quantity = 1 } ]
      let actualSanta = newModel.SantasList
      actualChildren == expectedChild
      actualSanta == expectedSanta

    testCase "Can add duplicate item to different child" <| fun () ->
      let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child1; child2 ] }
      let item = { Description = "Book" }

      let newModel =
        addItem "Dave" item model
        |> addItem "Shaw" item

      let expectedChildren = [
        { child1 with NaughtyOrNice = Nice [ item ] }
        { child2 with NaughtyOrNice = Nice [ item ] }
      ]
      let actualChildren = newModel.ChildrensList
      let expectedSanta = [ { ItemName = "Book"; Quantity = 2 } ]
      let actualSanta = newModel.SantasList
      actualChildren == expectedChildren
      actualSanta == expectedSanta


    testCase "Adding subsequent items to a child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child ] }

      let item1 = { Description = "Book" }
      let item2 = { Description = "Hat" }
      let item3 = { Description = "Scarf" }

      let newModel =
        addItem "Dave" item1 model
        |> addItem "Dave" item2
        |> addItem "Dave" item3

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


    testCase "Adding duplicate casing of items items to a child" <| fun () ->
      let child = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child ] }

      let item1 = { Description = "Book" }
      let item2 = { Description = "book" }

      let newModel =
        addItem "Dave" item1 model
        |> addItem "Dave" item2

      let expectedChild = { child with NaughtyOrNice = Nice [ item1; ] }
      let actualChild = newModel.ChildrensList |> List.head
      let expectedSanta = [
        { ItemName = "Book"; Quantity = 1 }
      ]
      let actualSanta = newModel.SantasList
      actualChild == expectedChild
      actualSanta == expectedSanta

    testCase "Adding duplicate casing of items items to different children" <| fun () ->
      let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child1; child2 ] }
      let item1 = { Description = "Book" }
      let item2 = { Description = "book" }

      let newModel =
        addItem "Dave" item1 model
        |> addItem "Shaw" item2

      let expectedChildren = [
        { child1 with NaughtyOrNice = Nice [ item1 ] }
        { child2 with NaughtyOrNice = Nice [ item2 ] }
      ]
      let actualChildren = newModel.ChildrensList
      let expectedSanta = [ { ItemName = "Book"; Quantity = 2 } ]
      let actualSanta = newModel.SantasList
      actualChildren == expectedChildren
      actualSanta == expectedSanta

    testCase "Ensure leading/trailing whitespace is removed on items" <| fun () ->
      let child1 = { Name = "Dave"; NaughtyOrNice = Nice [] }
      let child2 = { Name = "Shaw"; NaughtyOrNice = Nice [] }
      let model = { defaultModel with ChildrensList = [ child1; child2 ] }
      let item1 = { Description = " book" }
      let item2 = { Description = "book " }

      let newModel =
        addItem "Dave" item1 model
        |> addItem "Shaw" item2

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

    testCase "Ensure leading/trailing whitespace is removed on children" <| fun () ->
      let modelAfter1 = addChild " Dave" defaultModel
      let modelAfter2 = addChild "Dave " modelAfter1

      let expectedChildren = [ { Name = "Dave"; NaughtyOrNice = Undecided } ]

      modelAfter1 == modelAfter2
      modelAfter2.ChildrensList == expectedChildren
  ]

let tests =
  testList "Domain Tests" (testCases)
