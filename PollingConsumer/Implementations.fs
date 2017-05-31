// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.Imp

open System
open Ploeh.Samples.PollingConsumer
open Ploeh.Samples.ColorPrint

let shouldIdle idleDuration stopBefore (nm : NoMessageData) =
    nm.Stopped + idleDuration < stopBefore

let idle (idleDuration : TimeSpan) () =
    let s () =
        idleDuration.TotalMilliseconds
        |> int
        |> Async.Sleep
        |> Async.RunSynchronously
    cprintfn ConsoleColor.Yellow "Sleeping"
    Timed.timeOn Clocks.machineClock s ()

let shouldPoll calculateExpectedDuration stopBefore (r : ReadyData) =
    let durations = r.Result
    let expectedHandleDuration = calculateExpectedDuration durations
    r.Stopped + expectedHandleDuration < stopBefore

let poll pollForMessage handle clock () =
    let p () =
        match pollForMessage () with
        | Some msg ->
            let h () = Timed.timeOn clock (handle >> ignore) msg
            Some { Handle = h }
        | None -> None
    Timed.timeOn clock p ()