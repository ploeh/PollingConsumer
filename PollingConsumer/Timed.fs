// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

namespace Ploeh.Samples

open System

type Timed<'a> =
    {
        Started : DateTimeOffset
        Stopped : DateTimeOffset
        Result : 'a
    }
    member this.Duration = this.Stopped - this.Started

module Untimed =
    let map f x =
        { Started = x.Started; Stopped = x.Stopped; Result = f x.Result }

    let withResult newResult x = map (fun _ -> newResult) x

module Timed =
    let capture clock x =
        let now = clock ()
        { Started = now; Stopped = now; Result = x }

    let map clock f x =
        let result = f x.Result
        let stopped = clock ()
        { Started = x.Started; Stopped = stopped; Result = result }

    let timeOn clock f x = x |> capture clock |> map clock f

module Clocks =
    let machineClock () = DateTimeOffset.Now

    let acclock (start : DateTimeOffset) rate () =
        let now = DateTimeOffset.Now
        let elapsed = now - start
        start.AddTicks (elapsed.Ticks * rate)

    open System.Collections.Generic

    let qlock (q : Queue<DateTimeOffset>) = q.Dequeue

    let seqlock (l : DateTimeOffset seq) = Queue<DateTimeOffset> l |> qlock