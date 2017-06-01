// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.Imp

open System
open Ploeh.Samples.ColorPrint

let time f x =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start ()
    let msg = f x
    sw.Stop ()
    msg, sw.Elapsed

let idle (IdleDuration d) =
    let s () =
        d.TotalMilliseconds
        |> int
        |> Async.Sleep
        |> Async.RunSynchronously
    cprintfn ConsoleColor.Yellow "Sleeping"
    let (), actualDuration = time s ()
    IdleDuration actualDuration

let shouldPoll estimatedDuration stopBefore statistics = polling {
    let toTotalCycleTimeSpan x =
        let (PollDuration pd) = x.PollDuration
        let (HandleDuration hd) = x.HandleDuration
        pd + hd
    let expectedHandleDuration =
        statistics
        |> List.map toTotalCycleTimeSpan
        |> Statistics.calculateExpectedDuration estimatedDuration
    let! now = Polling.currentTime
    return now + expectedHandleDuration < stopBefore }
