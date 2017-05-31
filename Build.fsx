#r @"packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

Target "Build" <| fun _ ->
    !! "PollingConsumer.sln"
    |> MSBuildRelease "" "Rebuild"
    |> ignore

Target "Test" <| fun _ ->
    !! "*Tests/bin/Release/*Tests.dll"
    |> xUnit2 id

"Build"
==> "Test"

RunTargetOrDefault "Test"