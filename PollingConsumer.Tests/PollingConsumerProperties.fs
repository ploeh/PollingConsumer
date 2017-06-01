// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

module Ploeh.Samples.PollingConsumerProperties

open System
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open PollingConsumer

[<Property(QuietOnSuccess = true)>]
let ``transitionFromNoMessage returns correct result when it should idle``
    (nm : NoMessageData)
    (idleRes : Timed<unit>) =

    let shouldIdle _ = true
    let idle _ = idleRes

    let actual : State = transitionFromNoMessage shouldIdle idle nm

    let expected = idleRes |> Untimed.withResult nm.Result |> ReadyState
    expected =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromNoMessage returns correct result when it shouldn't idle``
    (nm : NoMessageData)
    (idleRes : Timed<unit>) =

    let shouldIdle _ = false
    let idle _ = idleRes

    let actual = transitionFromNoMessage shouldIdle idle nm

    StoppedState nm.Result =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when it shouldn't poll``
    (r : ReadyData)
    (mh : Timed<MessageHandler option>) =

    let shouldPoll _ = false
    let poll _ = mh

    let actual : State = transitionFromReady shouldPoll poll r
    
    StoppedState r.Result =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when polling no message``
    (r : ReadyData)
    (mh : Timed<unit>) =
    
    let shouldPoll _ = true
    let poll _ = mh |> Untimed.withResult None

    let actual = transitionFromReady shouldPoll poll r

    let expected = mh |> Untimed.withResult r.Result |> NoMessageState
    expected =! actual

[<Property(QuietOnSuccess = true)>]
let ``transitionFromReady returns correct result when polling a message``
    (r : ReadyData)
    (mh : Timed<MessageHandler>) =

    let shouldPoll _ = true
    let poll _ = mh |> Untimed.withResult (Some mh.Result)

    let actual = transitionFromReady shouldPoll poll r

    let expected =
        mh
        |> Untimed.withResult (r.Result, mh.Result)
        |> ReceivedMessageState
    expected =! actual
