// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.Statistics

open System

let calculateAverage (durations : TimeSpan list) =
    if durations.IsEmpty
    then None
    else
        durations
        |> List.averageBy (fun x -> float x.Ticks)
        |> int64
        |> TimeSpan.FromTicks
        |> Some

let calculateAverageAndStandardDeviation durations =
    let stdDev (avg : TimeSpan) =
        durations
        |> List.averageBy (fun x -> ((x - avg).Ticks |> float) ** 2.)
        |> sqrt
        |> int64
        |> TimeSpan.FromTicks
    durations |> calculateAverage |> Option.map (fun avg -> avg, stdDev avg)

// The expected duration is the average duration, plus three standard
// deviations. Assuming a normal distribution of durations, this should include
// 99.7 % of all durations. If the list of durations is empty, this function
// falls back on an estimated duration that the caller must supply as a wild
// guess.
let calculateExpectedDuration estimatedDuration durations =
    match calculateAverageAndStandardDeviation durations with
    | None -> estimatedDuration
    | Some (avg, stdDev) -> avg + stdDev + stdDev + stdDev // avg + stdDev * 3

