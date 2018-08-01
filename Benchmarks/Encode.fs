namespace FleeceB.Benchmarks

open Fleece
open BenchmarkDotNet.Attributes
open System.Text

module Examples =
    module E = Fleece.Serialization.Json.Encode
    module EI = Fleece.Inference.Json.Encode
    module Inline =
        module Explicit =
            type Testing =
                { one: int option
                  two: bool
                  three: int }
                static member Encode (x: Testing, jObj: JsonObject): JsonObject =
                    jObj
                    |> E.optional E.int "1" x.one
                    |> E.required E.bool "2" x.two
                    |> E.required E.int "3" x.three
                static member ToJson (x: Testing): Json =
                    Testing.Encode (x, JsonObject.empty)
                    |> E.jsonObject
            let testObject = { one = None; two = true; three = 42 }

        module Inferred =
            type Testing =
                { one: int option
                  two: bool
                  three: int }
                static member Encode (x: Testing, jObj: JsonObject): JsonObject =
                    jObj
                    |> EI.optional "1" x.one
                    |> EI.required "2" x.two
                    |> EI.required "3" x.three
                static member ToJson (x: Testing): Json =
                    Testing.Encode (x, JsonObject.empty)
                    |> Fleece.Inference.Json.encode
            let testObject = { one = None; two = true; three = 42 }

    module InModule =
        module Explicit =
            type Testing =
                { one: int option
                  two: bool
                  three: int }
            module Testing =
                let encode x jObj =
                    jObj
                    |> E.optional E.int "1" x.one
                    |> E.required E.bool "2" x.two
                    |> E.required E.int "3" x.three
            type Testing with
                static member Encode (x: Testing, jObj: JsonObject): JsonObject =
                    Testing.encode x jObj
                static member ToJson (x: Testing): Json =
                    E.buildWith Testing.encode x
            let testObject = { one = None; two = true; three = 42 }

        module Inferred =
            type Testing =
                { one: int option
                  two: bool
                  three: int }
            module Testing =
                let encode x jObj =
                    jObj
                    |> EI.optional "1" x.one
                    |> EI.required "2" x.two
                    |> EI.required "3" x.three
            type Testing with
                static member ToJson (x: Testing): Json =
                    E.buildWith Testing.encode x
            let testObject = { one = None; two = true; three = 42 }

[<Config(typeof<CoreConfig>)>]
type Encoding () =
    [<Benchmark>]
    member x.Inline_Explicit () =
        Inference.Json.encode Examples.Inline.Explicit.testObject

    [<Benchmark(Baseline=true)>]
    member x.InModule_Explicit () =
        Inference.Json.encode Examples.InModule.Explicit.testObject

    [<Benchmark>]
    member x.Inline_Inferred () =
        Inference.Json.encode Examples.Inline.Inferred.testObject

    [<Benchmark>]
    member x.InModule_Inferred () =
        Inference.Json.encode Examples.InModule.Inferred.testObject

module E = Samples.Json.Encode

[<Config(typeof<CoreConfig>)>]
type EncodeMedium () =
    [<Benchmark>]
    member x.EncodeA () =
        E.complexType Samples.Constants.objects.[0]

    [<Benchmark>]
    member x.EncodeB () =
        E.complexType Samples.Constants.objects.[1]
