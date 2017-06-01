// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.Imp

open System
open Ploeh.Samples.ColorPrint

let private time f x =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start ()
    let msg = f x
    sw.Stop ()
    msg, sw.Elapsed

let private r = Random ()

let poll =
    time (Simulation.pollForMessage r) >> fun (msg, d) -> msg, PollDuration d

let handle = time (Simulation.handle r) >> snd >> HandleDuration

let idle (IdleDuration d) =
    let s () =
        d.TotalMilliseconds
        |> int
        |> Async.Sleep
        |> Async.RunSynchronously
    cprintfn ConsoleColor.Yellow "Sleeping"
    time s () |> snd |> IdleDuration
