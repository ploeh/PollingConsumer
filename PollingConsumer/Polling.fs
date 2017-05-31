// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

namespace Ploeh.Samples

open System

type PollDuration = PollDuration of TimeSpan
type IdleDuration = IdleDuration of TimeSpan
type HandleDuration = HandleDuration of TimeSpan
type CycleDuration = {
    PollDuration : PollDuration
    HandleDuration : HandleDuration }

type PollingInstruction<'msg, 'next> =
| CurrentTime of (DateTimeOffset -> 'next)
| Poll of (('msg option * PollDuration) -> 'next)
| Handle of ('msg * (HandleDuration -> 'next))
| Idle of (IdleDuration * (IdleDuration -> 'next))

type PollingProgram<'msg, 'next> =
| Free of PollingInstruction<'msg, PollingProgram<'msg, 'next>>
| Pure of 'next

module Polling =
    // Underlying functor
    let private mapI f = function
        | CurrentTime next -> CurrentTime (next >> f)
        | Poll next -> Poll (next >> f)
        | Handle (x, next) -> Handle (x, next >> f)
        | Idle (x, next) -> Idle (x, next >> f)

    let rec bind f = function
        | Free instruction -> instruction |> mapI (bind f) |> Free
        | Pure x -> f x

    let map f = bind (f >> Pure)

    let currentTime = Free (CurrentTime Pure)

    let poll = Free (Poll Pure)

    let handle msg = Free (Handle (msg, Pure))

    let idle duration = Free (Idle (duration, Pure))

type PollingBuilder () =
    member this.Bind (x, f) = Polling.bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = this.Return ()

[<AutoOpen>]
module PollingComputationExpression =
    let polling = PollingBuilder ()
