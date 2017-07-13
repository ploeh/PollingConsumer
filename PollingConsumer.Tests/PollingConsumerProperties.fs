// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.PollingConsumerProperties

open System
open Hedgehog
open Xunit
open Swensen.Unquote
open PollingConsumer

type Responses = {
    CurrentTime : DateTimeOffset
    Poll : unit option * PollDuration
    Handle : HandleDuration
    Idle : IdleDuration }

let createInterpreter responses =
    let rec interpret = function
        | Pure x -> x
        | Free (CurrentTime next) -> responses.CurrentTime |> next |> interpret
        | Free (Poll next)        -> responses.Poll        |> next |> interpret
        | Free (Handle (_, next)) -> responses.Handle      |> next |> interpret
        | Free (Idle (_, next))   -> responses.Idle        |> next |> interpret
    interpret

module Gen =
    // A generator of 'moderate' positive TimeSpan values. It uses 32-bit
    // integers to stay within a reasonable range. The addition of 1 prevents
    // the generation of zero TimeSpans.
    let moderateTimeSpan =
        Range.constantBounded ()
        |> Gen.int
        |> Gen.map (abs >> ((+) 1) >> int64 >> TimeSpan.FromTicks)

    let cycleDuration = gen {
        let! (pd, hd) = Gen.tuple moderateTimeSpan
        return { PollDuration = PollDuration pd
                 HandleDuration = HandleDuration hd } }

    let responses =
        let timeSpan =
            Range.constantBounded ()
            |> Gen.int
            |> Gen.map (int64 >> TimeSpan.FromTicks)

        gen {
            let! currentTime = Gen.dateTime |> Gen.map DateTimeOffset
            let! poll = timeSpan |> Gen.map PollDuration |> Gen.zip (Gen.constant <| Some ())
            let! handle = timeSpan |> Gen.map HandleDuration
            let! idle = timeSpan |> Gen.map IdleDuration

            return { CurrentTime = currentTime
                     Poll = poll
                     Handle = handle
                     Idle = idle } }

[<Fact>]
let ``transitionFromNoMessage returns correct result when it has time to idle`` () =
    Property.check <| property {
        let! statistics = Gen.list (Range.linear 0 100) Gen.cycleDuration
        let! responses = Gen.responses
        let! idleDuration = Gen.moderateTimeSpan
        let! margin = Gen.moderateTimeSpan
        let stopBefore = responses.CurrentTime + idleDuration + margin

        let actual =
            transitionFromNoMessage
                (IdleDuration idleDuration) stopBefore (statistics, ())
            |> createInterpreter responses

        let expected = ReadyState statistics
        return expected = actual
    }

[<Fact>]
let ``transitionFromNoMessage returns correct result when it has no time to idle`` () =
    Property.check <| property {
        let! statistics = Gen.list (Range.linear 0 100) Gen.cycleDuration
        let! responses = Gen.responses
        let! idleDuration = Gen.moderateTimeSpan
        let! x = Gen.moderateTimeSpan
        let stopBefore = responses.CurrentTime + idleDuration - x

        let actual =
            transitionFromNoMessage
                (IdleDuration idleDuration) stopBefore (statistics, ())
            |> createInterpreter responses
        return StoppedState statistics = actual
    }

[<Fact>]
let ``transitionFromReady returns correct result when it has no time to cycle`` () =
    Property.check <| property {
        let! statistics = Gen.list (Range.linear 0 100) Gen.cycleDuration
        let! responses = Gen.responses
        let! estimatedDuration = Gen.moderateTimeSpan
        let! x = Gen.moderateTimeSpan
        let expectedDuration =
            statistics
            |> List.map PollingConsumer.toTotalCycleTimeSpan
            |> Statistics.calculateExpectedDuration estimatedDuration
        let stopBefore = responses.CurrentTime + expectedDuration - x

        let actual =
            transitionFromReady estimatedDuration stopBefore statistics
            |> createInterpreter responses    

        return StoppedState statistics = actual
    }

[<Fact>]
let ``transitionFromReady returns correct result when polling no message`` () =
    Property.check <| property {
        let! statistics = Gen.list (Range.linear 0 100) Gen.cycleDuration
        let! responses = Gen.responses
        let! estimatedDuration = Gen.moderateTimeSpan
        let! margin = Gen.moderateTimeSpan
        let responses = { responses with Poll = (None, snd responses.Poll) }
        let expectedDuration =
            statistics
            |> List.map PollingConsumer.toTotalCycleTimeSpan
            |> Statistics.calculateExpectedDuration estimatedDuration
        let stopBefore = responses.CurrentTime + expectedDuration + margin

        let actual =
            transitionFromReady estimatedDuration stopBefore statistics
            |> createInterpreter responses

        let expected = NoMessageState (statistics, snd responses.Poll)
        return expected = actual
    }

[<Fact>]
let ``transitionFromReady returns correct result when polling a message`` () =
    Property.check <| property {
        let! statistics = Gen.list (Range.linear 0 100) Gen.cycleDuration
        let! responses = Gen.responses
        let! estimatedDuration = Gen.moderateTimeSpan
        let! margin = Gen.moderateTimeSpan
        let responses = { responses with Poll = (Some (), snd responses.Poll) }
        let expectedDuration =
            statistics
            |> List.map PollingConsumer.toTotalCycleTimeSpan
            |> Statistics.calculateExpectedDuration estimatedDuration
        let stopBefore = responses.CurrentTime + expectedDuration + margin

        let actual =
            transitionFromReady estimatedDuration stopBefore statistics
            |> createInterpreter responses

        let expected = ReceivedMessageState (statistics, snd responses.Poll, ())
        return expected = actual
    }
