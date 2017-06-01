// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.PollingConsumerProperties

open System
open FsCheck
open FsCheck.Xunit
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
        Arb.generate |> Gen.map (abs >> ((+) 1) >> int64 >> TimeSpan.FromTicks)

[<Property(QuietOnSuccess = true)>]
let ``transitionFromNoMessage returns correct result when it has time to idle``
    statistics
    responses =

    Gen.moderateTimeSpan
    |> Gen.two
    |> Arb.fromGen
    |> Prop.forAll <| fun (idleDuration, margin) ->
        let stopBefore = responses.CurrentTime + idleDuration + margin

        let actual =
            transitionFromNoMessage
                (IdleDuration idleDuration) stopBefore (statistics, ())
            |> createInterpreter responses

        let expected = ReadyState statistics
        expected =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromNoMessage returns correct result when it has no time to idle``
    statistics
    responses =

    Gen.moderateTimeSpan
    |> Gen.two
    |> Arb.fromGen
    |> Prop.forAll <| fun (idleDuration, x) ->
        let stopBefore = responses.CurrentTime + idleDuration - x
        
        let actual =
            transitionFromNoMessage
                (IdleDuration idleDuration) stopBefore (statistics, ())
            |> createInterpreter responses
        StoppedState statistics =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when it shouldn't poll``
    statistics
    responses =

    let shouldPoll _ = polling { return false }
    let actual =
        transitionFromReady shouldPoll statistics
        |> createInterpreter responses    
    StoppedState statistics =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when polling no message``
    statistics
    responses =
    
    let responses = { responses with Poll = (None, snd responses.Poll) }
    let shouldPoll _ = polling { return true }

    let actual =
        transitionFromReady shouldPoll statistics
        |> createInterpreter responses

    let expected = NoMessageState (statistics, snd responses.Poll)
    expected =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when polling a message``
    statistics
    responses =

    let responses = { responses with Poll = (Some (), snd responses.Poll) }
    let shouldPoll _ = polling { return true }

    let actual =
        transitionFromReady shouldPoll statistics
        |> createInterpreter responses

    let expected = ReceivedMessageState (statistics, snd responses.Poll, ())
    expected =! actual
