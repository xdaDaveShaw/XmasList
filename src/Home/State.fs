module Home.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  { Name= ""; Accepted= false}, []

let update msg model : Model * Cmd<Msg> =
  match msg with
  | ChangeStr str ->
    { model with Name = str }, []
  | AcceptName ->
    { model with Accepted = true }, []
