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
  |> Shell.cleanDirs
)
let inline withWorkDir wd =
  DotNet.Options.withWorkingDirectory wd

Target.create "Restore" (fun _ ->
  DotNet.exec (withWorkDir "./src") "restore" ""
  |> ignore
)

Target.create "YarnInstall" (fun _ ->
  Yarn.install (fun opts -> opts)
  |> ignore
)

Target.create "Build" (fun _ ->
  DotNet.exec (withWorkDir "./src") "fable" "yarn-run build"
  |> ignore
)

Target.create "TestBuild" (fun _ ->
  DotNet.exec (withWorkDir "./src") "fable" "yarn-run test-build"
  |> ignore
)

Target.create "Test" (fun _ ->
  Yarn.exec "run test" (fun opts -> opts)
  |> ignore
)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "YarnInstall"
  ==> "Build"
  ==> "TestBuild"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
