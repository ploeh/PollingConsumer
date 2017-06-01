// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.PollingConsumer

// States
type State<'msg> =
| ReadyState of CycleDuration list
| ReceivedMessageState of (CycleDuration list * PollDuration * 'msg)
| NoMessageState of (CycleDuration list * PollDuration)
| StoppedState of CycleDuration list

// Transitions
let transitionFromNoMessage shouldIdle d (statistics, _) = polling {
    let! b = shouldIdle
    if b then
        do! Polling.idle d |> Polling.map ignore
        return ReadyState statistics
    else return StoppedState statistics }

let transitionFromReady shouldPoll statistics = polling {
    let! b = shouldPoll statistics
    if b then
        let! pollResult = Polling.poll
        match pollResult with
        | Some msg, pd -> return ReceivedMessageState (statistics, pd, msg)
        | None, pd -> return NoMessageState (statistics, pd)
    else return StoppedState statistics }

let transitionFromReceived (statistics, pd, msg) = polling {
    let! hd = Polling.handle msg
    return
        { PollDuration = pd; HandleDuration = hd } :: statistics
        |> ReadyState }

let transitionFromStopped s = polling { return StoppedState s }

// 'UI'
let durations = function
    | ReadyState statistics                   -> statistics
    | ReceivedMessageState (statistics, _, _) -> statistics
    | NoMessageState (statistics, _)          -> statistics
    | StoppedState statistics                 -> statistics

// State machine
let transition shouldPoll shouldIdle idleDuration state =
    match state with
    | ReadyState s -> transitionFromReady shouldPoll s
    | ReceivedMessageState s -> transitionFromReceived s
    | NoMessageState s -> transitionFromNoMessage shouldIdle idleDuration s
    | StoppedState s -> transitionFromStopped s
