module Util

#if FABLE_COMPILER
open Fable.Import.Jest
open Fable.Import.Jest.Matchers

let testList (name: string) (tests: (string * obj) seq) =
  name, tests

let testCase (msg: string) (test: unit->unit) =
  msg, box test

let (==) =
  (==)

#else
open Expecto

let testList (name: string) (tests : Test list) =
  testList name tests

let testCase (name: string) (test: unit -> unit) : Test =
  testCase name test

let (==) x y =
  Expect.equal y x ""

#endif
