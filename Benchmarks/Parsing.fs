namespace FleeceB.Benchmarks

open BenchmarkDotNet.Attributes
open Fleece
module Bench =
    open System.IO
    open System.Text

    let resetStream (stream : #Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore

    module SystemJson =        
        open Fleece.SystemJson.Operators
        open Fleece.Decode        
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwithf "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse

    module SystemTextJson =
        open Fleece.SystemTextJson.Operators
        open Fleece.Decode        
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwithf "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse

    module NewtonsoftJson =
        open Fleece.Newtonsoft.Operators
        open Fleece.Decode        
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwithf "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse

    module FSharpData =
        open Fleece.FSharpData.Operators
        open Fleece.Decode
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwithf "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse



[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    let mutable jsonString = null

    [<GlobalSetup>]
    member this.Setup () =
        jsonString <- loadJsonResourceAsString this.Name

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.SystemJson () =
        Bench.SystemJson.parse jsonString

    [<Benchmark>]
    member __.SystemTextJson () =
        Bench.SystemTextJson.parse jsonString

    [<Benchmark>]
    member __.NewtonsoftJson () =
        Bench.NewtonsoftJson.parse jsonString

    [<Benchmark>]
    member __.FSharpData () =
        Bench.FSharpData.parse jsonString