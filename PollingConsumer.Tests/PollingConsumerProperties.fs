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

// Execution

[<Property(QuietOnSuccess = true)>]
let ``unfurl returns correct sequence with constant transition``
    (initialValue : string)
    (constantValue : string)
    (count : byte) =

    let getNext _ = constantValue
    let actual : string seq = unfurl getNext initialValue
    test <@
            actual
            |> Seq.skip 1
            |> Seq.truncate (int count)
            |> Seq.forall ((=) constantValue) @>

[<Property(QuietOnSuccess = true)>]
let ``unfurl returns as many values as requested``
    (initialValue : TimeSpan)
    (count : byte) =

    let actual = unfurl id initialValue
    int count =! (actual |> Seq.truncate (int count) |> Seq.length)

[<Property(QuietOnSuccess = true)>]
let ``unfurl returns correct values``
    (initialValue : byte)
    (count : byte) =

    let actual = unfurl ((+) 1) (int initialValue)

    let expected = [int initialValue .. int initialValue + int count]
    expected =! (actual |> Seq.truncate (int count + 1) |> Seq.toList)

[<Property(QuietOnSuccess = true)>]
let ``run returns last element of sequence without stops`` () =
    let ss =
        Arb.generate<State>
        |> Gen.filter (not << isStopped)
        |> Gen.listOf
        |> Arb.fromGen
        |> Arb.filter (not << List.isEmpty)
    Prop.forAll ss (fun states ->

        let actual : State = run states
    
        let expected = states |> Seq.last
        expected =! actual)

[<Property(QuietOnSuccess = true)>]
let ``run returns StoppedState when it occurs`` (states : State list) =
    states |> List.exists isStopped ==> lazy

    let actual : State = run states
    
    let expected = states |> List.find isStopped
    expected =! actual
