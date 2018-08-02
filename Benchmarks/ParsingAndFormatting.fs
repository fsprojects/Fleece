namespace FleeceB.Benchmarks

open BenchmarkDotNet.Attributes
open Fleece
module Bench =
    open System.IO
    open System.Text

    let resetStream (stream : #Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore

    module SystemJson =
        open FleeceSystemJson
        open System.Json
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse

    module NewtonsoftJson =
        open FleeceNewtonsoftJson
        open Newtonsoft.Json.Linq
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse

    module FSharpData =
        open FleeceFSharpData
        open FSharp.Data
        let inline parse (s: string): JsonObject =
            s
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 

        let inline parseStream (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parse



[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    let mutable jsonStream = null
    let mutable jsonString = null

    [<Setup>]
    member this.Setup () =
        jsonString <- loadJsonResourceAsString this.Name
        //jsonStream <- loadJsonResource this.Name

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.SystemJson () =
        Bench.SystemJson.parse jsonString

    [<Benchmark>]
    member __.NewtonsoftJson () =
        Bench.resetStream jsonStream
        Bench.NewtonsoftJson.parse jsonString

    [<Benchmark>]
    member __.FSharpData () =
        Bench.resetStream jsonStream
        Bench.FSharpData.parse jsonString

