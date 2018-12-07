#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.JavaScript.Yarn
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.JavaScript

Target.create "Clean" (fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  ++ "tests/output"
  ++ "deploy"
  |> Shell.cleanDirs
)
let inline withWorkDir wd =
  DotNet.Options.withWorkingDirectory wd

Target.create "Restore" (fun _ ->
  DotNet.exec (withWorkDir "./src") "restore" ""
  |> ignore
)

Target.create "Hack" (fun _ ->
  let home = System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
  let matcher = System.IO.Path.Combine(home, ".nuget", "packages", "fable.import.jest", "1.9.0", "fable", "Matchers.fs")
  System.IO.File.Copy("./hack/Matchers.fs", matcher, true)
)

Target.create "YarnInstall" (fun _ ->
  Yarn.install (fun opts -> opts)
  |> ignore
)

Target.create "Build" (fun _ ->
  Yarn.exec "run build" (fun opts -> opts)
  |> ignore
)

Target.create "TestBuild-Jest" (fun _ ->
  Yarn.exec "run test-build" (fun opts -> opts)
  |> ignore
)

Target.create "Test-Jest" (fun _ ->
  Yarn.exec "run test" (fun opts -> opts)
  |> ignore
)

Target.create "Test-dotnet" (fun _ ->
  DotNet.exec (withWorkDir "./tests") "run" "--project XmasList.Tests.fsproj"
  |> ignore
)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Hack"
  ==> "YarnInstall"
  ==> "Build"
  ==> "TestBuild-Jest"
  ==> "Test-Jest"
  ==> "Test-dotnet"
  ==> "All"

Target.runOrDefault "All"
