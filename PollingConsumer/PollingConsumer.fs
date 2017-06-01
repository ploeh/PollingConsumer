// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.PollingConsumer

open System

// Auxiliary types
[<CustomEquality; NoComparison>]
type MessageHandler =
    { Handle : unit -> Timed<unit> } 

    override this.Equals obj =
        match obj with
        | :? MessageHandler as other ->
            Object.Equals(this.Handle, other.Handle)
        | _ -> false

    override this.GetHashCode() = (box this.Handle).GetHashCode()

// State data
type ReadyData = Timed<TimeSpan list>
type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>
type NoMessageData = Timed<TimeSpan list>
type StoppedData = TimeSpan list

// States
type State =
| ReadyState of ReadyData
| ReceivedMessageState of ReceivedMessageData
| NoMessageState of NoMessageData
| StoppedState of StoppedData

// Transitions
let transitionFromNoMessage shouldIdle idle nm =
    if shouldIdle nm
    then idle () |> Untimed.withResult nm.Result |> ReadyState
    else StoppedState nm.Result

let transitionFromReady shouldPoll poll r =
    if shouldPoll r
    then 
        let msg = poll ()
        match msg.Result with
        | Some h -> msg |> Untimed.withResult (r.Result, h) |> ReceivedMessageState
        | None -> msg |> Untimed.withResult r.Result |> NoMessageState
    else StoppedState r.Result

let transitionFromReceived (rm : ReceivedMessageData) =
    let durations, messageHandler = rm.Result
    let t = messageHandler.Handle ()
    let pollDuration = rm.Duration
    let handleDuration = t.Duration
    let totalDuration = pollDuration + handleDuration
    t |> Untimed.withResult (totalDuration :: durations) |> ReadyState

let transitionFromStopped s = StoppedState s

// State machine

let transition shouldPoll poll shouldIdle idle state =
    match state with
    | ReadyState r -> transitionFromReady shouldPoll poll r
    | ReceivedMessageState rm -> transitionFromReceived rm
    | NoMessageState nm -> transitionFromNoMessage shouldIdle idle nm
    | StoppedState s -> transitionFromStopped s

let rec run shouldPoll poll shouldIdle idle state =
    let ns = transition shouldPoll poll shouldIdle idle state
    match ns with
    | StoppedState _ -> ns
    | _              -> run shouldPoll poll shouldIdle idle ns

let startOn clock = [] |> Timed.capture clock |> ReadyState

let durations = function
    | ReadyState r -> r.Result
    | ReceivedMessageState rm -> fst rm.Result
    | NoMessageState nm -> nm.Result
    | StoppedState s -> s
