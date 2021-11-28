﻿namespace Fleece.SystemTextJson

open System
open System.Collections.Generic
open System.Text.Json
open System.Globalization
open FSharpPlus
open FSharpPlus.Data
open Fleece


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>]
module Internals =
    // pseudo-AST, wrapping Encoding subtypes:
    let inline (|JArray|JObject|JNumber|JBool|JString|JNull|) (j: ^Encoding) =
        let o = (^Encoding : (member InnerValue : JsonElement) j)
        match o.ValueKind with
        | JsonValueKind.Null
        | JsonValueKind.Undefined -> JNull
        | JsonValueKind.Array     -> JArray ([ for x in o.EnumerateArray () -> (^Encoding : (static member Wrap: _ -> ^Encoding) x) ] |> Seq.toList)
        | JsonValueKind.Object    -> JObject (Map.ofList [for x in o.EnumerateObject () -> (x.Name, (^Encoding : (static member Wrap: _ -> ^Encoding) x.Value))] :> IReadOnlyDictionary<_,_>)
        | JsonValueKind.Number    -> JNumber j
        | JsonValueKind.False     -> JBool false
        | JsonValueKind.True      -> JBool true
        | JsonValueKind.String    -> JString (o.GetString ())
        | _                       -> failwithf "Invalid Encoding %A" o


open Internals

type JsonObject = Map<string, Encoding>


/// Wrapper type for JsonElement
and [<Struct>]Encoding = { mutable Value : Choice<JsonElement, Utf8JsonWriter -> string option-> unit>  } with

    static member Wrap x = { Encoding.Value = Choice1Of2 x }

    member this.ToString (options: JsonWriterOptions) =
        use stream = new System.IO.MemoryStream ()
        use writer = new Utf8JsonWriter (stream, options)
        use reader = new System.IO.StreamReader (stream)
        match this with
        | { Value = Choice2Of2 jobj  } -> jobj writer None
        | { Value = Choice1Of2 value } -> value.WriteTo writer
        writer.Flush ()
        stream.Seek (0L, System.IO.SeekOrigin.Begin) |> ignore
        reader.ReadToEnd ()

    override this.ToString () = this.ToString (new JsonWriterOptions (Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping))

    static member Parse (x: string) = let doc = JsonDocument.Parse x in { Value = Choice1Of2 doc.RootElement }

    member this.get_InnerValue () =
        match this with
        | { Value = Choice1Of2 value } -> value
        | { Value = Choice2Of2 _ } ->
            // run the function, then parseback
            let str = string this
            let doc = JsonDocument.Parse str
            let value = doc.RootElement
            this.Value <- Choice1Of2 value
            value

    member this.getWriter () =
        match this with
        | { Value = Choice2Of2 writer } -> writer
        | { Value = Choice1Of2 value  } ->
            fun (writer: Utf8JsonWriter) (name: string option) ->
                name |> Option.iter writer.WritePropertyName
                value.WriteTo writer

    static member inline private writers keyValueWriter valueWriter = { Value = Choice2Of2 (fun (writer: Utf8JsonWriter) -> function Some name -> keyValueWriter writer name | _ -> valueWriter writer) }

    static member inline JArray (x: Encoding IReadOnlyList) =
        let f w =
            for v in x do (v.getWriter ()) w None
            w.WriteEndArray ()
        Encoding.writers (fun w k -> w.WriteStartArray k; f w) (fun w -> w.WriteStartArray (); f w)

    static member inline JObject (x: IReadOnlyDictionary<string, Encoding>) =
        let f w =
            for kv in x do kv.Value.getWriter () w (Some kv.Key)
            w.WriteEndObject ()
        Encoding.writers (fun w k -> w.WriteStartObject k; f w) (fun w -> w.WriteStartObject (); f w)

    static member JBool (x: bool)      = Encoding.writers (fun w k -> w.WriteBoolean (k, x)) (fun w -> w.WriteBooleanValue x)
    static member JNull                = Encoding.writers (fun w k -> w.WriteNull k) (fun w -> w.WriteNullValue ())
    static member JString (x: string)  = if isNull x then Encoding.JNull else Encoding.writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
    static member JNumber (x: decimal) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)

    static member jsonObjectGetValues (o: JsonObject) = o :> IReadOnlyDictionary<string, Encoding>

    static member dictAsJsonObject (x: IReadOnlyDictionary<string, Encoding>) =
        match x with
        | :? JsonObject as x' -> x'
        | _ -> x |> Seq.map (|KeyValue|) |> Array.ofSeq |> JsonObject

    /// Creates a new Json object for serialization
    static member jobj (x: seq<string * Encoding>) : Encoding = Encoding.JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> Map.ofSeq)

    static member create (x: string ) = Encoding.writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
    static member create (x: Guid   ) = Encoding.writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
    static member create (x: decimal) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: Single ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: Double ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: int    ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: int64  ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: uint32 ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: uint64 ) = Encoding.writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
    static member create (x: int16  ) = Encoding.writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
    static member create (x: uint16 ) = Encoding.writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
    static member create (x: byte   ) = Encoding.writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
    static member create (x: sbyte  ) = Encoding.writers (fun w k -> w.WriteNumber (k,  int32 x)) (fun w -> w.WriteNumberValue ( int32 x))
    static member create (x: char   ) = Encoding.writers (fun w k -> w.WriteString (k, string x)) (fun w -> w.WriteStringValue (string x))

    static member ($) (_:Encoding, _: decimal) = fun (x: Encoding) -> x.get_InnerValue().GetDecimal ()
    static member ($) (_:Encoding, _: int16  ) = fun (x: Encoding) -> x.get_InnerValue().GetInt16 ()
    static member ($) (_:Encoding, _: int    ) = fun (x: Encoding) -> x.get_InnerValue().GetInt32 ()
    static member ($) (_:Encoding, _: int64  ) = fun (x: Encoding) -> x.get_InnerValue().GetInt64 ()
    static member ($) (_:Encoding, _: uint16 ) = fun (x: Encoding) -> x.get_InnerValue().GetUInt16 ()
    static member ($) (_:Encoding, _: uint32 ) = fun (x: Encoding) -> x.get_InnerValue().GetUInt32 ()
    static member ($) (_:Encoding, _: uint64 ) = fun (x: Encoding) -> x.get_InnerValue().GetUInt64 ()
    static member ($) (_:Encoding, _: byte   ) = fun (x: Encoding) -> x.get_InnerValue().GetByte ()
    static member ($) (_:Encoding, _: sbyte  ) = fun (x: Encoding) -> x.get_InnerValue().GetSByte ()
    static member ($) (_:Encoding, _: float  ) = fun (x: Encoding) -> x.get_InnerValue().GetDouble ()
    static member ($) (_:Encoding, _: float32) = fun (x: Encoding) -> x.get_InnerValue().GetSingle ()

    static member inline tryGet (x: Encoding) : 't = (Unchecked.defaultof<Encoding> $ Unchecked.defaultof<'t>) x

    static member inline tryRead x =
        match x with
        | JNumber j ->
            try 
                Ok (Encoding.tryGet j)
            with e -> Decode.Fail.invalidValue x (string e)
        | js -> Decode.Fail.numExpected js

    /// Downcasts IEncoding to a SystemTextJson.Encoding
    static member Unwrap (x: IEncoding) = x :?> Encoding

    static member toIEncoding (c: Codec<Encoding, 't>) : Codec<IEncoding, 't> = c |> Codec.upCast
    static member ofIEncoding (c: Codec<IEncoding, 't>) : Codec<Encoding, 't> = c |> Codec.downCast


    static member jsonObjectOfJson = function
        | JObject x -> Ok (Encoding.dictAsJsonObject x)
        | a -> Decode.Fail.objExpected a

    static member jsonOfJsonObject (o: JsonObject) = Encoding.JObject o

    static member createTuple c t = function
        | JArray a as x -> if List.length a <> c then Decode.Fail.count c x else t a
        | a -> Decode.Fail.arrExpected a

    
    //////////////
    // Decoders //
    //////////////

    static member resultD (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) : Encoding -> ParseResult<Result<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Ok", a)]    -> a |> decoder1 |> Result.map Ok
            | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
            | _ -> Decode.Fail.invalidValue (jobj: Encoding) ""
        | a -> Decode.Fail.objExpected a

    static member choiceD (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) : Encoding -> ParseResult<Choice<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
            | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
            | _ -> Decode.Fail.invalidValue jobj ""
        | a -> Decode.Fail.objExpected a

    static member choice3D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) : Encoding -> ParseResult<Choice<'a, 'b, 'c>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
            | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
            | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
            | _ -> Decode.Fail.invalidValue jobj ""
        | a     -> Decode.Fail.objExpected a

    static member optionD (decoder: Encoding -> ParseResult<'a>) : Encoding -> ParseResult<'a option> = function
        | JNull _ -> Ok None
        | x       -> Result.map Some (decoder x)

    static member nullableD (decoder: Encoding -> ParseResult<'a>) : Encoding -> ParseResult<Nullable<'a>> = function
        | JNull _ -> Ok (Nullable ())
        | x       -> Result.map Nullable (decoder x)

    static member arrayD (decoder: Encoding -> ParseResult<'a>) : Encoding -> ParseResult<'a array> = function
        | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected a
        
    static member propListD (decoder: Encoding -> ParseResult<'a>) : Encoding -> ParseResult<PropertyList<'a>> = function
        | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toArray |> PropertyList)
        | a         -> Decode.Fail.objExpected a

    static member unitD : Encoding -> ParseResult<unit> =
        Encoding.createTuple 0 (fun _ -> (Ok ()))

    static member tuple1D (decoder1: Encoding -> ParseResult<'a>) : Encoding -> ParseResult<Tuple<'a>> =
        Encoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

    static member tuple2D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) : Encoding -> ParseResult<'a * 'b> =
        Encoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

    static member tuple3D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) : Encoding -> ParseResult<'a * 'b * 'c> =
        Encoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
    static member tuple4D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) (decoder4: Encoding -> ParseResult<'d>) : Encoding -> ParseResult<'a * 'b * 'c * 'd> =
        Encoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
    static member tuple5D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) (decoder4: Encoding -> ParseResult<'d>) (decoder5: Encoding -> ParseResult<'e>) : Encoding -> ParseResult<'a * 'b * 'c * 'd * 'e> =
        Encoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
    static member tuple6D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) (decoder4: Encoding -> ParseResult<'d>) (decoder5: Encoding -> ParseResult<'e>) (decoder6: Encoding -> ParseResult<'f>) : Encoding -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
        Encoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
    static member tuple7D (decoder1: Encoding -> ParseResult<'a>) (decoder2: Encoding -> ParseResult<'b>) (decoder3: Encoding -> ParseResult<'c>) (decoder4: Encoding -> ParseResult<'d>) (decoder5: Encoding -> ParseResult<'e>) (decoder6: Encoding -> ParseResult<'f>) (decoder7: Encoding -> ParseResult<'g>) : Encoding -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
        Encoding.createTuple 7 (fun a -> Result.map (fun a b c d e f g -> (a, b, c, d, e, f, g)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6])

    static member decimalD x = Encoding.tryRead<decimal> x
    static member int16D   x = Encoding.tryRead<int16>   x
    static member intD     x = Encoding.tryRead<int>     x
    static member int64D   x = Encoding.tryRead<int64>   x
    static member uint16D  x = Encoding.tryRead<uint16>  x
    static member uint32D  x = Encoding.tryRead<uint32>  x
    static member uint64D  x = Encoding.tryRead<uint64>  x
    static member byteD    x = Encoding.tryRead<byte>    x
    static member sbyteD   x = Encoding.tryRead<sbyte>   x
    static member floatD   x = Encoding.tryRead<double>  x
    static member float32D x = Encoding.tryRead<single>  x

    static member enumD x : Result< 't, _> when 't: enum<_> =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue x s
        | a -> Decode.Fail.strExpected a

    static member booleanD x =
        match x with
        | JBool b -> Ok b
        | a -> Decode.Fail.boolExpected a

    static member stringD x =
        match x with
        | JString b -> Ok b
        | JNull     -> Ok null
        | a -> Decode.Fail.strExpected a

    static member charD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> Ok s.[0]
        | a -> Decode.Fail.strExpected a

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue x s
        | a -> Decode.Fail.strExpected a

    static member dateTimeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue x ""
        | a -> Decode.Fail.strExpected a

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue x ""
        | a -> Decode.Fail.strExpected a

    static member timeSpanD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JNumber _ as j -> Encoding.int64D j |> Result.map TimeSpan
        | a -> Decode.Fail.numExpected a


    //////////////
    // Encoders //
    //////////////

    static member resultE (encoder1: _ -> Encoding) (encoder2: _ -> Encoding) = function
        | Ok    a -> jobj [ "Ok"   , encoder1 a ]
        | Error a -> jobj [ "Error", encoder2 a ]

    static member choiceE (encoder1: _ -> Encoding) (encoder2: _ -> Encoding) = function
        | Choice1Of2 a -> jobj [ "Choice1Of2", encoder1 a ]
        | Choice2Of2 a -> jobj [ "Choice2Of2", encoder2 a ]

    static member choice3E (encoder1: _ -> Encoding) (encoder2: _ -> Encoding) (encoder3: _ -> Encoding) = function
        | Choice1Of3 a -> jobj [ "Choice1Of3", encoder1 a ]
        | Choice2Of3 a -> jobj [ "Choice2Of3", encoder2 a ]
        | Choice3Of3 a -> jobj [ "Choice3Of3", encoder3 a ]

    static member optionE (encoder: _ -> Encoding) = function
        | None   -> Encoding.JNull
        | Some a -> encoder a

    static member nullableE (encoder: _ -> Encoding) (x: Nullable<'a>) = if x.HasValue then encoder x.Value else Encoding.JNull    
    static member arrayE    (encoder: _ -> Encoding) (x: 'a [])        = Encoding.JArray ((Array.map encoder x) |> Array.toList)
    static member propListE (encoder: _ -> Encoding) (x: PropertyList<'a>) = x |> filter (fun (k, _) -> not (isNull k)) |> map encoder |> Encoding.JObject

    static member tuple1E (encoder1: 'a -> Encoding) (a: Tuple<_>) = Encoding.JArray ([|encoder1 a.Item1|] |> Seq.toList)
    static member tuple2E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (a, b) = Encoding.JArray ([|encoder1 a; encoder2 b|] |> Seq.toList)
    static member tuple3E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (encoder3: 'c -> Encoding) (a, b, c) = Encoding.JArray ([|encoder1 a; encoder2 b; encoder3 c|] |> Seq.toList)
    static member tuple4E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (encoder3: 'c -> Encoding) (encoder4: 'd -> Encoding) (a, b, c, d) = Encoding.JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d|] |> Seq.toList)
    static member tuple5E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (encoder3: 'c -> Encoding) (encoder4: 'd -> Encoding) (encoder5: 'e -> Encoding) (a, b, c, d, e) = Encoding.JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e|] |> Seq.toList)
    static member tuple6E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (encoder3: 'c -> Encoding) (encoder4: 'd -> Encoding) (encoder5: 'e -> Encoding) (encoder6: 'f -> Encoding) (a, b, c, d, e, f) = Encoding.JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f|] |> Seq.toList)
    static member tuple7E (encoder1: 'a -> Encoding) (encoder2: 'b -> Encoding) (encoder3: 'c -> Encoding) (encoder4: 'd -> Encoding) (encoder5: 'e -> Encoding) (encoder6: 'f -> Encoding) (encoder7: 'g -> Encoding) (a, b, c, d, e, f, g) = Encoding.JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f; encoder7 g|] |> Seq.toList)
    
    static member enumE (x: 't when 't: enum<_>) = Encoding.JString (string x)
    static member unitE () = Encoding.JArray []

    static member booleanE        (x: bool          ) = Encoding.JBool x
    static member stringE         (x: string        ) = Encoding.JString x
    static member dateTimeE       (x: DateTime      ) = Encoding.JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffZ"))
    static member dateTimeOffsetE (x: DateTimeOffset) = Encoding.JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffK"))
    static member timeSpanE       (x: TimeSpan) = Encoding.create x.Ticks

    static member decimalE        (x: decimal       ) = Encoding.create x
    static member floatE          (x: Double        ) = Encoding.create x
    static member float32E        (x: Single        ) = Encoding.create x
    static member intE            (x: int           ) = Encoding.create x
    static member uint32E         (x: uint32        ) = Encoding.create x
    static member int64E          (x: int64         ) = Encoding.create x
    static member uint64E         (x: uint64        ) = Encoding.create x
    static member int16E          (x: int16         ) = Encoding.create x
    static member uint16E         (x: uint16        ) = Encoding.create x
    static member byteE           (x: byte          ) = Encoding.create x
    static member sbyteE          (x: sbyte         ) = Encoding.create x
    static member charE           (x: char          ) = Encoding.create x
    static member guidE           (x: Guid          ) = Encoding.create x

    
    ////////////
    // Codecs //
    ////////////

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.resultD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.resultE (Codec.encode codec1) (Codec.encode codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.choiceD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.choiceE (Codec.encode codec1) (Codec.encode codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = Encoding.choice3D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) <-> Encoding.choice3E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3)
    static member option   (codec: Codec<_,_>) = Encoding.optionD (Codec.decode codec) <-> Encoding.optionE (Codec.encode codec)
    static member nullable (codec: Codec<Encoding, 't>) = Encoding.nullableD (Codec.decode codec) <-> Encoding.nullableE (Codec.encode codec) : Codec<Encoding, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = Encoding.arrayD    (Codec.decode codec) <-> Encoding.arrayE    (Codec.encode codec)
    static member propList (codec: Codec<_,_>) = Encoding.propListD (Codec.decode codec) <-> Encoding.propListE (Codec.encode codec)

    static member unit = Encoding.unitD <-> Encoding.unitE
    static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = Encoding.tuple1D (Codec.decode codec1)                                                                                                                                     <-> Encoding.tuple1E (Codec.encode codec1)
    static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = Encoding.tuple2D (Codec.decode codec1) (Codec.decode codec2)                                                                                                               <-> Encoding.tuple2E (Codec.encode codec1) (Codec.encode codec2)
    static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = Encoding.tuple3D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3)                                                                                         <-> Encoding.tuple3E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3)
    static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = Encoding.tuple4D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4)                                                                   <-> Encoding.tuple4E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4)
    static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = Encoding.tuple5D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5)                                             <-> Encoding.tuple5E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5)
    static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = Encoding.tuple6D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5) (Codec.decode codec6)                       <-> Encoding.tuple6E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5) (Codec.encode codec6)
    static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = Encoding.tuple7D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5) (Codec.decode codec6) (Codec.decode codec7) <-> Encoding.tuple7E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5) (Codec.encode codec6) (Codec.encode codec7)

    static member boolean        = Encoding.booleanD        <-> Encoding.booleanE
    static member string         = Encoding.stringD         <-> Encoding.stringE
    static member dateTime       = Encoding.dateTimeD       <-> Encoding.dateTimeE
    static member dateTimeOffset = Encoding.dateTimeOffsetD <-> Encoding.dateTimeOffsetE
    static member timeSpan       = Encoding.timeSpanD       <-> Encoding.timeSpanE
    static member decimal        = Encoding.decimalD        <-> Encoding.decimalE
    static member float          = Encoding.floatD          <-> Encoding.floatE
    static member float32        = Encoding.float32D        <-> Encoding.float32E
    static member int            = Encoding.intD            <-> Encoding.intE
    static member uint32         = Encoding.uint32D         <-> Encoding.uint32E
    static member int64          = Encoding.int64D          <-> Encoding.int64E
    static member uint64         = Encoding.uint64D         <-> Encoding.uint64E
    static member int16          = Encoding.int16D          <-> Encoding.int16E
    static member uint16         = Encoding.uint16D         <-> Encoding.uint16E
    static member byte           = Encoding.byteD           <-> Encoding.byteE
    static member sbyte          = Encoding.sbyteD          <-> Encoding.sbyteE
    static member char           = Encoding.charD           <-> Encoding.charE
    static member guid           = Encoding.guidD           <-> Encoding.guidE


    interface IEncoding with
        member _.unit           = Encoding.toIEncoding Encoding.unit
        member _.boolean        = Encoding.toIEncoding Encoding.boolean
        member _.string         = Encoding.toIEncoding Encoding.string
        member _.dateTime       = Encoding.toIEncoding Encoding.dateTime
        member _.dateTimeOffset = Encoding.toIEncoding Encoding.dateTimeOffset
        member _.timeSpan       = Encoding.toIEncoding Encoding.timeSpan
        member _.decimal        = Encoding.toIEncoding Encoding.decimal
        member _.float          = Encoding.toIEncoding Encoding.float
        member _.float32        = Encoding.toIEncoding Encoding.float32
        member _.int            = Encoding.toIEncoding Encoding.int
        member _.uint32         = Encoding.toIEncoding Encoding.uint32
        member _.int64          = Encoding.toIEncoding Encoding.int64
        member _.uint64         = Encoding.toIEncoding Encoding.uint64
        member _.int16          = Encoding.toIEncoding Encoding.int16
        member _.uint16         = Encoding.toIEncoding Encoding.uint16
        member _.byte           = Encoding.toIEncoding Encoding.byte
        member _.sbyte          = Encoding.toIEncoding Encoding.sbyte
        member _.char           = Encoding.toIEncoding Encoding.char
        member _.guid           = Encoding.toIEncoding Encoding.guid

        member _.result c1 c2     = Encoding.toIEncoding (Encoding.result   (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2))
        member _.choice c1 c2     = Encoding.toIEncoding (Encoding.choice   (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2))
        member _.choice3 c1 c2 c3 = Encoding.toIEncoding (Encoding.choice3  (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3))
        member _.option c         = Encoding.toIEncoding (Encoding.option   (Encoding.ofIEncoding c))
        member _.array c          = Encoding.toIEncoding (Encoding.array    (Encoding.ofIEncoding c))
        member _.propertyList c   = Encoding.toIEncoding (Encoding.propList (Encoding.ofIEncoding c))

        member _.tuple1 c                    = Encoding.toIEncoding (Encoding.tuple1 (Encoding.ofIEncoding c))
        member _.tuple2 c1 c2                = Encoding.toIEncoding (Encoding.tuple2 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2))
        member _.tuple3 c1 c2 c3             = Encoding.toIEncoding (Encoding.tuple3 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3))
        member _.tuple4 c1 c2 c3 c4          = Encoding.toIEncoding (Encoding.tuple4 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3) (Encoding.ofIEncoding c4))
        member _.tuple5 c1 c2 c3 c4 c5       = Encoding.toIEncoding (Encoding.tuple5 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3) (Encoding.ofIEncoding c4) (Encoding.ofIEncoding c5))
        member _.tuple6 c1 c2 c3 c4 c5 c6    = Encoding.toIEncoding (Encoding.tuple6 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3) (Encoding.ofIEncoding c4) (Encoding.ofIEncoding c5) (Encoding.ofIEncoding c6))
        member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = Encoding.toIEncoding (Encoding.tuple7 (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3) (Encoding.ofIEncoding c4) (Encoding.ofIEncoding c5) (Encoding.ofIEncoding c6) (Encoding.ofIEncoding c7))

        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> (_: Codec<IEncoding, 'u>) : Codec<IEncoding, 't> = Encoding.toIEncoding (Encoding.enumD <-> Encoding.enumE)

        member x.getCase =
            match x with
            | JNull     -> "JNull"
            | JBool   _ -> "JBool" 
            | JNumber _ -> "JNumber"
            | JString _ -> "JString"
            | JArray  _ -> "JArray"
            | JObject _ -> "JObject"

[<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>]
module InternalHelpers =
    let inline JArray x  = Encoding.JArray x
    let inline JObject x = Encoding.JObject x
    let        JBool x   = Encoding.JBool x
    let        JNull     = Encoding.JNull
    let        JString x = Encoding.JString x
    let        JNumber x = Encoding.JNumber x