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

// Support functions
let toTotalCycleTimeSpan x =
    let (PollDuration pd) = x.PollDuration
    let (HandleDuration hd) = x.HandleDuration
    pd + hd

let private shouldIdle (IdleDuration d) stopBefore = polling {
    let! now = Polling.currentTime
    return now + d < stopBefore }

let private shouldPoll estimatedDuration stopBefore statistics = polling {
    let expectedHandleDuration =
        statistics
        |> List.map toTotalCycleTimeSpan
        |> Statistics.calculateExpectedDuration estimatedDuration
    let! now = Polling.currentTime
    return now + expectedHandleDuration < stopBefore }

// Transitions
let transitionFromNoMessage d stopBefore (statistics, _) = polling {
    let! b = shouldIdle d stopBefore
    if b then
        do! Polling.idle d |> Polling.map ignore
        return ReadyState statistics
    else return StoppedState statistics }

let transitionFromReady estimatedDuration stopBefore statistics = polling {
    let! b = shouldPoll estimatedDuration stopBefore statistics
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
let transition estimatedDuration idleDuration stopBefore = function
    | ReadyState s -> transitionFromReady estimatedDuration stopBefore s
    | ReceivedMessageState s -> transitionFromReceived s
    | NoMessageState s -> transitionFromNoMessage idleDuration stopBefore s
    | StoppedState s -> transitionFromStopped s
