module XmasList.TestMain

open XmasList.Tests

#if FABLE_COMPILER
#else

open Expecto

let allTests =
  [
    domainTests
  ]

[<EntryPoint>]
let main args =
  allTests
  |> testList "All"
  |> runTestsWithArgs defaultConfig args

#endif
