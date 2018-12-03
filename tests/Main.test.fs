module TestsMain


let allTests =
  [|
    DomainTests.tests
  |]

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

let [<Global>] describe (name: string) (f: unit->unit) = jsNative
let [<Global>] it (msg: string) (f: unit->unit) = jsNative

let run () =
    for (name, tests) in allTests do
        describe name (fun () ->
            for (msg, test) in tests do
                it msg (unbox test))
run()
#else

open Expecto

[<EntryPoint>]
let main args =
  let writeResults = Expecto.TestResults.writeNUnitSummary ("../nunit.xml", "XmasList.Tests")
  let config = defaultConfig.appendSummaryHandler writeResults

  allTests
  |> Array.toList
  |> testList "All"
  |> runTestsWithArgs config args
#endif
