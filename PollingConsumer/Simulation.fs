// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.Simulation

open System
open Ploeh.Samples.ColorPrint

let pollForMessage (r : Random) () =
    printfn "Polling"

    r.Next(100, 1000) |> Async.Sleep |> Async.RunSynchronously

    if r.Next(0, 100) < 50 then Some () else None

let handle (r : Random) () =
    cprintfn ConsoleColor.Green "Handling"

    r.Next(100, 1000) |> Async.Sleep |> Async.RunSynchronously

