[<AutoOpen>]
module Extensions

open Elmish

module Cmd =
  let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
    let delayedCmd (dispatch: 'msg -> unit) : unit =
      let delayedDispatch = async {
          let! msg = operation
          dispatch msg
      }

      Async.StartImmediate delayedDispatch

    Cmd.ofSub delayedCmd

module Async =
  let map f (computation: Async<'t>) =
    async {
      let! x = computation
      return f x
    }

type Deferred<'t> =
  | HasNotStartedYet
  | InProgress
  | Resolved of 't

type AsyncOperationEvent<'t> =
  | Started
  | Finished of 't