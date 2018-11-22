module XmasList.Tests

open Fable.Import.Jest // See https://github.com/jgrund/fable-jest
open Types

test "Demo test" <| fun () ->
    expect.Invoke(sprintf "%d" 42).toEqual("42")

test "tofail" <| fun () ->
  expect.Invoke("asd").toEqual("asd")

test "xmas" <| fun () ->
  let a = XmasList.Types.Nice []
  let b = XmasList.Types.Nice []
  expect.Invoke(a).toEqual(b)

test "xmas2" <| fun () ->
  let model = fst (XmasList.State.init())
  let child = { Name = ""; NaughtyOrNice = Undecided }
  let newModel = Domain.addChild model child
  expect.Invoke(newModel.ChildrensList.Length).toEqual(0)
