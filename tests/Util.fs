module Util

#if FABLE_COMPILER
open Fable.Import.Jest
open Fable.Import.Jest.Matchers

let tests (name: string) (tests: (string * obj) seq) =
  name, tests

let test (name: string) (test: unit -> unit) =
  name, box test

let (==) =
  (==)

#else
open Expecto

let tests (name: string) (tests : Test list) =
  testList name tests

let test (name: string) (test: unit -> unit) : Test =
  testCase name test

let (==) x y =
  Expect.equal y x ""

#endif
