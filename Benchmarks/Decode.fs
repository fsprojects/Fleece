namespace FleeceB.Benchmarks

open Fleece
open BenchmarkDotNet.Attributes
open System.Text

module DecodeExamples =
    module E = Fleece.Serialization.Json.Encode
    module D = Fleece.Serialization.Json.Decode
    module DI = Fleece.Inference.Json.Decode
    open Fleece.Operators
    let testJson =
        JsonObject.ofPropertyList
            [ "2", E.bool true
              "3", E.int 42 ]
        |> JsonObject.optimizeRead
        |> E.jsonObject
    module Inline =
        module Explicit =
            module ComputationExpression =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member FromJson (_:Testing): Decoder<Json,Testing> = jsonDecoder {
                        let! o = D.optional D.int "1"
                        let! t = D.required D.bool "2"
                        let! r = D.required D.int "3"
                        return { one = o; two = t; three = r }
                    }

            module Operators =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member Decode (jObj: JsonObject): JsonResult<Testing> =
                        ((fun o t r -> { one = o; two = t; three = r })
                        <!> D.optional D.int "1"
                        <*> D.required D.bool "2"
                        <*> D.required D.int "3") jObj
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        let decode jObj = Testing.Decode(jObj)
                        D.jsonObject >=> decode

        module Inferred =
            module ComputationExpression =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member FromJson (_:Testing): Decoder<Json,Testing> = jsonDecoder {
                        let! o = DI.optional "1"
                        let! t = DI.required "2"
                        let! r = DI.required "3"
                        return { one = o; two = t; three = r }
                    }

            module Operators =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                    static member Decode (jObj: JsonObject): JsonResult<Testing> =
                        ((fun o t r -> { one = o; two = t; three = r })
                        <!> DI.optional "1"
                        <*> DI.required "2"
                        <*> DI.required "3") jObj
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        let decode jObj = Testing.Decode(jObj)
                        D.jsonObject >=> decode

    module InModule =
        module Explicit =
            module ComputationExpression =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let toJson = jsonDecoder {
                        let! o = D.optional D.int "1"
                        let! t = D.required D.bool "2"
                        let! r = D.required D.int "3"
                        return { one = o; two = t; three = r }
                    }
                type Testing with
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        Testing.toJson

            module Operators =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let decode =
                        (fun o t r -> { one = o; two = t; three = r })
                        <!> D.optional D.int "1"
                        <*> D.required D.bool "2"
                        <*> D.required D.int "3"
                    let toJson =
                        D.jsonObject >=> decode
                type Testing with
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        Testing.toJson

        module Inferred =
            module ComputationExpression =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let toJson = jsonDecoder {
                        let! o = DI.optional "1"
                        let! t = DI.required "2"
                        let! r = DI.required "3"
                        return { one = o; two = t; three = r }
                    }
                type Testing with
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        Testing.toJson

            module Operators =
                type Testing =
                    { one: int option
                      two: bool
                      three: int }
                module Testing =
                    let decode =
                        (fun o t r -> { one = o; two = t; three = r })
                        <!> DI.optional "1"
                        <*> DI.required "2"
                        <*> DI.required "3"
                    let toJson =
                        D.jsonObject >=> decode
                type Testing with
                    static member FromJson (_: Testing): Decoder<Json,Testing> =
                        Testing.toJson

[<Config(typeof<CoreConfig>)>]
type Decoding () =
    [<Benchmark>]
    member x.Inline_Explicit_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Explicit.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Explicit_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Explicit.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Inferred_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Inferred.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Inferred_ComputationExpression () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Inferred.ComputationExpression.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Explicit_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Explicit.Operators.Testing>) |> ignore

    [<Benchmark(Baseline=true)>]
    member x.InModule_Explicit_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Explicit.Operators.Testing>) |> ignore

    [<Benchmark>]
    member x.Inline_Inferred_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.Inline.Inferred.Operators.Testing>) |> ignore

    [<Benchmark>]
    member x.InModule_Inferred_Operators () =
        (Inference.Json.decode DecodeExamples.testJson : JsonResult<DecodeExamples.InModule.Inferred.Operators.Testing>) |> ignore

module E = Fleece.Serialization.Json.Encode
module D = Fleece.Serialization.Json.Decode

// [<Config(typeof<CoreConfig>)>]
// type DecodeList () =
//     let testList =
//         E.listWith E.number
//             [ for i in 1..10000 do
//                 yield string i ]

    // let array = ListDecoders.arrayWith D.number
    // let arrayAlt = ListDecoders.arrayWithAlt D.number
    // let arrayAlt2 = ListDecoders.arrayWithAlt2 D.number
    // let arrayAlt3 = ListDecoders.arrayWithAlt3 D.number

    // let list = D.listWith D.number
    // let listAlt = arrayAlt |> JsonReader.map List.ofArray
    // let listAlt2 = arrayAlt2 |> JsonReader.map List.ofArray
    // let listAlt3 = arrayAlt3 |> JsonReader.map List.ofArray

    // [<Setup>]
    // member x.Setup() =
    //     let result = arrayAlt3 testList
    //     match result with
    //     | JPass _ -> ()
    //     | _ -> failwith (JsonResult.summarize result)

    // [<Benchmark>]
    // member x.Current_To_Array () =
    //     array testList

    // [<Benchmark>]
    // member x.Current_To_List () =
    //     list testList

    // [<Benchmark>]
    // member x.Alt_To_Array () =
    //     arrayAlt testList

    // [<Benchmark>]
    // member x.Alt_To_List () =
    //     listAlt testList

    // [<Benchmark>]
    // member x.Alt2_To_Array () =
    //     arrayAlt2 testList

    // [<Benchmark>]
    // member x.Alt2_To_List () =
    //     listAlt2 testList

    // [<Benchmark>]
    // member x.Alt3_To_Array () =
    //     arrayAlt3 testList


// [<Config(typeof<CoreConfig>)>]
// type DecodeListWithErrors () =
//     let testList =
//         E.listWith E.string
//             [ for i in 1..10000 do
//                 yield (string i + "λ") ]

    // let array = ListDecoders.arrayWith D.number
    // let arrayAlt = ListDecoders.arrayWithAlt D.number
    // let arrayAlt2 = ListDecoders.arrayWithAlt2 D.number
    // let arrayAlt3 = ListDecoders.arrayWithAlt3 D.number

    // let list = D.listWith D.number
    // let listAlt = arrayAlt |> JsonReader.map List.ofArray
    // let listAlt2 = arrayAlt2 |> JsonReader.map List.ofArray
    // let listAlt3 = arrayAlt3 |> JsonReader.map List.ofArray

    // [<Setup>]
    // member x.Setup() =
    //     let result = arrayAlt3 testList
    //     match result with
    //     | JPass _ -> failwith "Should be failures, but were none"
    //     | _ -> ()

    // [<Benchmark>]
    // member x.Current_To_Array () =
    //     array testList

    // [<Benchmark>]
    // member x.Current_To_List () =
    //     list testList

    // [<Benchmark>]
    // member x.Alt_To_Array () =
    //     arrayAlt testList

    // [<Benchmark>]
    // member x.Alt_To_List () =
    //     listAlt testList

    // [<Benchmark>]
    // member x.Alt2_To_Array () =
    //     arrayAlt2 testList

    // [<Benchmark>]
    // member x.Alt2_To_List () =
    //     listAlt2 testList

    // [<Benchmark>]
    // member x.Alt3_To_Array () =
    //     arrayAlt3 testList

[<Config(typeof<CoreConfig>)>]
type DecodeJsonObjectToPropertyList () =
    let testObj =
        let rec inner i jObj =
            match i with
            | 0u -> jObj
            | _ -> JsonObject.add (string i) (E.uint32 i) jObj |> inner (i - 1u)
        inner 10000u JsonObject.empty

    let objectReader = JsonObject.toPropertyListWithCustomKey JsonResult.pass D.json

    [<Benchmark>]
    member x.WithTransferFunction () =
         objectReader testObj

module D = Samples.Json.Decode

[<Config(typeof<CoreConfig>)>]
type DecodeMedium () =
    [<Benchmark>]
    member x.DecodeA () =
        D.complexType Samples.Constants.jsons.[0]

    [<Benchmark>]
    member x.DecodeB () =
        D.complexType Samples.Constants.jsons.[1]

    [<Benchmark>]
    member x.DecodeC () =
        D.complexType Samples.Constants.jsons.[2]

    [<Benchmark>]
    member x.DecodeAWithError () =
        D.childType Samples.Constants.jsons.[0]

    [<Benchmark>]
    member x.DecodeBWithError () =
        D.childType Samples.Constants.jsons.[1]

    [<Benchmark>]
    member x.DecodeCWithError () =
        D.childType Samples.Constants.jsons.[2]
