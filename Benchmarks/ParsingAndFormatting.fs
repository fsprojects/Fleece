namespace FleeceB.Benchmarks

open BenchmarkDotNet.Attributes
open Fleece
module Bench =
    open System.IO
    open System.Text

    let resetStream (stream : #Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore

    module Fleece =
        open FleeceSystemJson
        let inline parse (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 

    module FleeceNewtonsoftJson =
        open FleeceNewtonsoftJson
        let inline parse (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 

    module FleeceFSharpData =
        open FleeceFSharpData
        let inline parse (stream: #Stream): JsonObject =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> parseJson
            |> function | Success v->v | Failure e ->failwith "%A" e 



[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    let mutable jsonStream = null
    let mutable jsonString = null

    [<Setup>]
    member this.Setup () =
        jsonString <- loadJsonResourceAsString this.Name
        jsonStream <- loadJsonResource this.Name

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.Fleece_New (): Fleece.JsonResult<Fleece.Json> =
        Fleece.Parsing.Json.parse jsonString

    [<Benchmark>]
    member __.Newtonsoft () =
        Bench.resetStream jsonStream
        Bench.JsonNET.parse jsonStream

