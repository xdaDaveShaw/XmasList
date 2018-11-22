#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let CWD = __SOURCE_DIRECTORY__

let srcDir = CWD </> "src"

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs
)
let inline withWorkDir wd =
  DotNet.Options.withWorkingDirectory wd

Target.create "Restore" (fun _ ->
  DotNet.exec (withWorkDir srcDir) "restore" ""
  |> ignore
)

Target.create "Build" (fun _ ->
    DotNet.exec (withWorkDir srcDir) "fable" "yarn-run build"
    |> ignore
)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "All"

Target.runOrDefault "All"
