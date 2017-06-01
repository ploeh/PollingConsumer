// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

open System
open Ploeh.Samples
open Ploeh.Samples.ColorPrint

let limit = TimeSpan.FromMinutes 1.

let composeDependencies (now : DateTimeOffset) =
    let stopBefore = now + limit
    let estimatedDuration = TimeSpan.FromSeconds 2.
    let idleDuration = TimeSpan.FromSeconds 5. |> IdleDuration
    let r = Random ()
    
    let shouldPoll = Imp.shouldPoll estimatedDuration stopBefore

    let handle =
        Imp.time (Simulation.handle r) >> fun (_, d) -> HandleDuration d

    let poll =
        Imp.time (Simulation.pollForMessage r)
        >> fun (msg, d) -> msg, PollDuration d

    let idle = Imp.idle

    shouldPoll, poll, idle, handle, idleDuration, stopBefore

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

let rec interpret pollImp handleImp idleImp = function
    | Pure x -> x
    | Free (CurrentTime next) ->
        DateTimeOffset.Now |> next |> interpret pollImp handleImp idleImp
    | Free (Poll next) ->
        pollImp () |> next |> interpret pollImp handleImp idleImp
    | Free (Handle (msg, next)) ->
        handleImp msg |> next |> interpret pollImp handleImp idleImp
    | Free (Idle (d, next)) ->
        idleImp d |> next |> interpret pollImp handleImp idleImp

let rec run shouldPoll idleDuration stopBefore pollImp handleImp idleImp state =
    let ns =
        PollingConsumer.transition shouldPoll idleDuration stopBefore state
        |> interpret pollImp handleImp idleImp 
    match ns with
    | PollingConsumer.StoppedState _ -> ns
    | _ -> run shouldPoll idleDuration stopBefore pollImp handleImp idleImp ns

let toTotalCycleTimeSpan x =
    let (PollDuration pd) = x.PollDuration
    let (HandleDuration hd) = x.HandleDuration
    pd + hd

[<EntryPoint>]
let main _ =
    let timeAtEntry = DateTimeOffset.Now

    printOnEntry timeAtEntry

    let shouldPoll, poll, idle, handle, idleDuration, stopBefore =
        composeDependencies timeAtEntry
    let durations =
        PollingConsumer.ReadyState []
        |> run shouldPoll idleDuration stopBefore poll handle idle
        |> PollingConsumer.durations
        |> List.map toTotalCycleTimeSpan
    
    printOnExit timeAtEntry durations

    // Return 0. This indicates success.
    0