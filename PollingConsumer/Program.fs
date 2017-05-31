// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

open System
open Ploeh.Samples
open Ploeh.Samples.ColorPrint

let limit = TimeSpan.FromMinutes 1.

let composeTransition (now : DateTimeOffset) =
    let stopBefore = now + limit
    let estimatedDuration = TimeSpan.FromSeconds 2.
    let idleDuration = TimeSpan.FromSeconds 5.
    
    let calculateExpectedDuration =
        Statistics.calculateExpectedDuration estimatedDuration
    let shouldPoll = Imp.shouldPoll calculateExpectedDuration stopBefore

    let r = Random ()
    let pollForMessage = Simulation.pollForMessage r
    let handle = Simulation.handle r
    let poll = Imp.poll pollForMessage handle Clocks.machineClock

    let shouldIdle = Imp.shouldIdle idleDuration stopBefore

    let idle = Imp.idle idleDuration

    PollingConsumer.transition shouldPoll poll shouldIdle idle

let printOnEntry (timeAtEntry : DateTimeOffset) =
    printfn "Started polling at %s." (timeAtEntry.ToString "T")
    printfn ""

let printOnExit timeAtEntry (durations : TimeSpan list) =
    let stats = Statistics.calculateAverageAndStandardDeviation durations

    let timeAtExit = DateTimeOffset.Now
    let elapsed = timeAtExit - timeAtEntry
    let durationColor =
        if elapsed <= limit then ConsoleColor.Green else ConsoleColor.Red

    printfn ""
    printfn "Stopped polling at %s." (timeAtExit.ToString "T")
    printf  "Elapsed time: "
    cprintf durationColor "%s" (elapsed.ToString "c")
    printfn "."
    printfn "Handled %i message(s)." durations.Length
    stats
    |> Option.map (fun (avg, stdDev) -> avg.ToString "T", stdDev.ToString "T")
    |> Option.iter (fun (avg, stdDev) ->
        printfn "Average duration: %s" avg
        printfn "Standard deviation: %s" stdDev)

[<EntryPoint>]
let main args =
    
    let timeAtEntry = DateTimeOffset.Now

    printOnEntry timeAtEntry

    let durations =
        PollingConsumer.startOn Clocks.machineClock
        |> PollingConsumer.unfurl (composeTransition timeAtEntry)
        |> PollingConsumer.run
        |> PollingConsumer.durations
    
    printOnExit timeAtEntry durations

    // Return 0. This indicates success.
    0