namespace Fleece

module SystemTextJson =

    open System
    open System.Collections.Generic
    open System.Text.Json

    /// Wrapper type for JsonElement
    type JsonValue = { mutable Value : Choice<JsonElement, Utf8JsonWriter -> string option-> unit> } with

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

        member this.getValue () =
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
                    

    type JsonObject = Map<string, JsonValue>

    module JsonValue =

        let inline private writers keyValueWriter valueWriter = { Value = Choice2Of2 (fun (writer: Utf8JsonWriter) -> function Some name -> keyValueWriter writer name | _ -> valueWriter writer) }

        let inline JArray (x: JsonValue list) =
            let f w =
                for v in x do (v.getWriter ()) w None
                w.WriteEndArray ()
            writers (fun w k -> w.WriteStartArray k; f w) (fun w -> w.WriteStartArray (); f w)

        let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) =
            let f w =
                for kv in x do kv.Value.getWriter () w (Some kv.Key)
                w.WriteEndObject ()
            writers (fun w k -> w.WriteStartObject k; f w) (fun w -> w.WriteStartObject (); f w)

        let JBool (x: bool)      = writers (fun w k -> w.WriteBoolean (k, x)) (fun w -> w.WriteBooleanValue x)
        let JNull                = writers (fun w k -> w.WriteNull k) (fun w -> w.WriteNullValue ())
        let JString (x: string)  = if isNull x then JNull else writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
        let JNumber (x: decimal) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)

        // pseudo-AST, wrapping JsonValue subtypes:
        let (|JArray|JObject|JNumber|JBool|JString|JNull|) (j: JsonValue) =
            let o = j.getValue ()
            match o.ValueKind with
            | JsonValueKind.Null
            | JsonValueKind.Undefined -> JNull
            | JsonValueKind.Array     -> JArray ([ for x in o.EnumerateArray () -> {Value = Choice1Of2 x} ] |> Seq.toList)
            | JsonValueKind.Object    -> JObject ( Map.ofList [for x in o.EnumerateObject () -> (x.Name, {Value = Choice1Of2 x.Value})] :> IReadOnlyDictionary<_,_>)
            | JsonValueKind.Number    -> JNumber j
            | JsonValueKind.False     -> JBool false
            | JsonValueKind.True      -> JBool true
            | JsonValueKind.String    -> JString (o.GetString ())
            | _                       -> failwithf "Invalid JsonValue %A" o

        let jsonObjectGetValues (o: JsonObject) = o :> IReadOnlyDictionary<string, JsonValue>

        let dictAsJsonObject (x: IReadOnlyDictionary<string, JsonValue>) =
            match x with
            | :? JsonObject as x' -> x'
            | _ -> x |> Seq.map (|KeyValue|) |> Array.ofSeq |> JsonObject

        /// Creates a new Json object for serialization
        let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> Map.ofSeq)

        type internal JsonHelpers () =
            static member create (x: string ) = writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
            static member create (x: Guid   ) = writers (fun w k -> w.WriteString (k, x)) (fun w -> w.WriteStringValue x)
            static member create (x: decimal) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: Single ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: Double ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: int    ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: int64  ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: uint32 ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: uint64 ) = writers (fun w k -> w.WriteNumber (k, x)) (fun w -> w.WriteNumberValue x)
            static member create (x: int16  ) = writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
            static member create (x: uint16 ) = writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
            static member create (x: byte   ) = writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
            static member create (x: sbyte  ) = writers (fun w k -> w.WriteNumber (k,  int32 x)) (fun w -> w.WriteNumberValue ( int32 x))        
            static member create (x: char   ) = writers (fun w k -> w.WriteString (k, string x)) (fun w -> w.WriteStringValue (string x))        

        type TryGet = TryGet with                
            static member ($) (TryGet, _: decimal) = fun (x: JsonValue) -> x.getValue().GetDecimal ()
            static member ($) (TryGet, _: int16  ) = fun (x: JsonValue) -> x.getValue().GetInt16 ()
            static member ($) (TryGet, _: int    ) = fun (x: JsonValue) -> x.getValue().GetInt32 ()
            static member ($) (TryGet, _: int64  ) = fun (x: JsonValue) -> x.getValue().GetInt64 ()
            static member ($) (TryGet, _: uint16 ) = fun (x: JsonValue) -> x.getValue().GetUInt16 ()
            static member ($) (TryGet, _: uint32 ) = fun (x: JsonValue) -> x.getValue().GetUInt32 ()
            static member ($) (TryGet, _: uint64 ) = fun (x: JsonValue) -> x.getValue().GetUInt64 ()
            static member ($) (TryGet, _: byte   ) = fun (x: JsonValue) -> x.getValue().GetByte ()
            static member ($) (TryGet, _: sbyte  ) = fun (x: JsonValue) -> x.getValue().GetSByte ()
            static member ($) (TryGet, _: float  ) = fun (x: JsonValue) -> x.getValue().GetDouble ()
            static member ($) (TryGet, _: float32) = fun (x: JsonValue) -> x.getValue().GetSingle ()

        let inline tryGet (x: JsonValue) : 't = (TryGet $ Unchecked.defaultof<'t>) x


    open System.Globalization
    open FSharpPlus
    open FSharpPlus.Data
    open Fleece
    open Fleece.Helpers
    open Fleece.Operators
    open JsonValue


    // This is here because of an F# compiler regression which might affect us depending on the specific F# version: https://github.com/dotnet/fsharp/issues/11344
    let nullableE (encoder: _ -> JsonValue) (x: Nullable<'a>) = if x.HasValue then encoder x.Value else JNull

    type [<Struct>] StjEncoding = StjEncoding of JsonValue with

        override this.ToString () = let (StjEncoding x) = this in x.ToString ()
        
        static member Parse (x: string) = StjEncoding (JsonValue.Parse x)
        
        static member inline tryRead x =
            match x with
            | JNumber j ->
                try 
                    Ok (tryGet j)
                with e -> Decode.Fail.invalidValue (StjEncoding x) (string e)
            | js -> Decode.Fail.numExpected (StjEncoding js)

        /// Unwraps the JsonValue inside an IEncoding
        static member Unwrap (x: IEncoding) = x :?> StjEncoding |> fun (StjEncoding s) -> s

        /// Wraps a JsonValue inside an IEncoding
        static member Wrap x = StjEncoding x :> IEncoding

        static member toIRawCodec (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((StjEncoding.Unwrap >> Ok) <-> StjEncoding.Wrap)
        static member ofIRawCodec (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((StjEncoding.Wrap >> Ok) <-> StjEncoding.Unwrap)


        static member jsonObjectOfJson = function
            | JObject x -> Ok (dictAsJsonObject x)
            | a -> Decode.Fail.objExpected (StjEncoding a)

        static member jsonOfJsonObject (o: JsonObject) = JObject o       

        static member createTuple c t = function 
            | JArray a as x -> if List.length a <> c then Decode.Fail.count c (StjEncoding x) else t a
            | a -> Decode.Fail.arrExpected (StjEncoding a)

    
        //////////////
        // Decoders //
        //////////////

        static member resultD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Result<'a, 'b>> = function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue ("Ok", a)] -> a |> decoder1 |> Result.map Ok
                | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
                | _ -> Decode.Fail.invalidValue (StjEncoding jobj) ""
            | a -> Decode.Fail.objExpected (StjEncoding a)

        static member choiceD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
                | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
                | _ -> Decode.Fail.invalidValue (StjEncoding jobj) ""
            | a -> Decode.Fail.objExpected (StjEncoding a)

        static member choice3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
                | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
                | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
                | _ -> Decode.Fail.invalidValue (StjEncoding jobj) ""
            | a     -> Decode.Fail.objExpected (StjEncoding a)

        static member optionD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
            | JNull _ -> Ok None
            | x       -> Result.map Some (decoder x)

        static member nullableD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
            | JNull _ -> Ok (Nullable ())
            | x       -> Result.map Nullable (decoder x)

        static member arrayD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
            | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
            | a        -> Decode.Fail.arrExpected (StjEncoding a)
        
        static member multiMapD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<MultiObj<'a>> = function
            | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toList |> List.map KeyValuePair |> multiMap)
            | a         -> Decode.Fail.objExpected (StjEncoding a)

        static member unitD : JsonValue -> ParseResult<unit> =
            StjEncoding.createTuple 0 (fun _ -> (Ok ()))

        static member tuple1D (decoder1: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Tuple<'a>> =
            StjEncoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

        static member tuple2D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> =
            StjEncoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

        static member tuple3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> =
            StjEncoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
        static member tuple4D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
            StjEncoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
        static member tuple5D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
            StjEncoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
        static member tuple6D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
            StjEncoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
        static member tuple7D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
            StjEncoding.createTuple 7 (fun a -> Result.map (fun a b c d e f g -> (a, b, c, d, e, f, g)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6])

        static member decimalD x = StjEncoding.tryRead<decimal> x
        static member int16D   x = StjEncoding.tryRead<int16>   x
        static member intD     x = StjEncoding.tryRead<int>     x
        static member int64D   x = StjEncoding.tryRead<int64>   x
        static member uint16D  x = StjEncoding.tryRead<uint16>  x
        static member uint32D  x = StjEncoding.tryRead<uint32>  x
        static member uint64D  x = StjEncoding.tryRead<uint64>  x
        static member byteD    x = StjEncoding.tryRead<byte>    x
        static member sbyteD   x = StjEncoding.tryRead<sbyte>   x
        static member floatD   x = StjEncoding.tryRead<double>  x
        static member float32D x = StjEncoding.tryRead<single>  x

        static member enumD x : Result< 't, _> when 't: enum<_> =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (StjEncoding x) s
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member booleanD x =
            match x with
            | JBool b -> Ok b
            | a -> Decode.Fail.boolExpected (StjEncoding a)

        static member stringD x =
            match x with
            | JString b -> Ok b
            | JNull     -> Ok null
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member charD x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    -> Ok s.[0]
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member guidD x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (StjEncoding x) s
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member dateTimeD x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    ->
                match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
                | true, t -> Ok t
                | _       -> Decode.Fail.invalidValue (StjEncoding x) ""
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member dateTimeOffsetD x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    ->
                match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
                | true, t -> Ok t
                | _       -> Decode.Fail.invalidValue (StjEncoding x) ""
            | a -> Decode.Fail.strExpected (StjEncoding a)

        static member timeSpanD x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JNumber _ as j -> StjEncoding.int64D j |> Result.map TimeSpan
            | a -> Decode.Fail.numExpected (StjEncoding a)


        //////////////
        // Encoders //
        //////////////

        static member resultE (encoder1: _ -> JsonValue) (encoder2: _ -> JsonValue) = function
            | Ok    a -> jobj [ "Ok"   , encoder1 a ]
            | Error a -> jobj [ "Error", encoder2 a ]

        static member choiceE (encoder1: _ -> JsonValue) (encoder2: _ -> JsonValue) = function
            | Choice1Of2 a -> jobj [ "Choice1Of2", encoder1 a ]
            | Choice2Of2 a -> jobj [ "Choice2Of2", encoder2 a ]

        static member choice3E (encoder1: _ -> JsonValue) (encoder2: _ -> JsonValue) (encoder3: _ -> JsonValue) = function
            | Choice1Of3 a -> jobj [ "Choice1Of3", encoder1 a ]
            | Choice2Of3 a -> jobj [ "Choice2Of3", encoder2 a ]
            | Choice3Of3 a -> jobj [ "Choice3Of3", encoder3 a ]

        static member optionE (encoder: _ -> JsonValue) = function
            | None   -> JNull
            | Some a -> encoder a
    
        static member arrayE    (encoder: _ -> JsonValue) (x: 'a [])        = JArray ((Array.map encoder x) |> Array.toList)
        static member multiMapE (encoder: _ -> JsonValue) (x: MultiObj<'a>) = x |> MultiMap.toList |> Seq.filter (fun (k, _) -> not (isNull k)) |> Seq.map (fun (k, v) -> k, encoder v) |> Map.ofSeq |> JObject

        static member tuple1E (encoder1: 'a -> JsonValue) (a: Tuple<_>) = JArray ([|encoder1 a.Item1|] |> Seq.toList)
        static member tuple2E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (a, b) = JArray ([|encoder1 a; encoder2 b|] |> Seq.toList)
        static member tuple3E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (a, b, c) = JArray ([|encoder1 a; encoder2 b; encoder3 c|] |> Seq.toList)
        static member tuple4E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (a, b, c, d) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d|] |> Seq.toList)
        static member tuple5E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (a, b, c, d, e) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e|] |> Seq.toList)
        static member tuple6E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (a, b, c, d, e, f) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f|] |> Seq.toList)
        static member tuple7E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (encoder7: 'g -> JsonValue) (a, b, c, d, e, f, g) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f; encoder7 g|] |> Seq.toList)
    
        // requires F# 5 -->
        static member enumE (x: 't when 't: enum<_>) = JString (string x)
        static member unitE () = JArray ([||] |> Seq.toList)

        static member booleanE        (x: bool          ) = JBool x
        static member stringE         (x: string        ) = JString x
        static member dateTimeE       (x: DateTime      ) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffZ"))
        static member dateTimeOffsetE (x: DateTimeOffset) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffK"))
        static member timeSpanE       (x: TimeSpan) = JsonHelpers.create x.Ticks

        static member decimalE        (x: decimal       ) = JsonHelpers.create x
        static member floatE          (x: Double        ) = JsonHelpers.create x
        static member float32E        (x: Single        ) = JsonHelpers.create x
        static member intE            (x: int           ) = JsonHelpers.create x
        static member uint32E         (x: uint32        ) = JsonHelpers.create x
        static member int64E          (x: int64         ) = JsonHelpers.create x
        static member uint64E         (x: uint64        ) = JsonHelpers.create x
        static member int16E          (x: int16         ) = JsonHelpers.create x
        static member uint16E         (x: uint16        ) = JsonHelpers.create x
        static member byteE           (x: byte          ) = JsonHelpers.create x
        static member sbyteE          (x: sbyte         ) = JsonHelpers.create x
        static member charE           (x: char          ) = JsonHelpers.create x
        static member guidE           (x: Guid          ) = JsonHelpers.create x

    
        ////////////
        // Codecs //
        ////////////

        static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = StjEncoding.resultD (dec codec1) (dec codec2) <-> StjEncoding.resultE (enc codec1) (enc codec2)

        static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = StjEncoding.choiceD (dec codec1) (dec codec2) <-> StjEncoding.choiceE (enc codec1) (enc codec2)
        static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = StjEncoding.choice3D (dec codec1) (dec codec2) (dec codec3) <-> StjEncoding.choice3E (enc codec1) (enc codec2) (enc codec3)
        static member option (codec: Codec<_,_>) = StjEncoding.optionD (dec codec) <-> StjEncoding.optionE (enc codec)
        static member nullable (codec: Codec<JsonValue, 't>) = StjEncoding.nullableD (dec codec) <-> nullableE (enc codec) : Codec<JsonValue, Nullable<'t>>
        static member array    (codec: Codec<_,_>) = StjEncoding.arrayD  (dec codec) <-> StjEncoding.arrayE    (enc codec)
        static member multiMap (codec: Codec<_,_>) = StjEncoding.multiMapD (dec codec) <-> StjEncoding.multiMapE (enc codec)

        static member unit () = StjEncoding.unitD <-> StjEncoding.unitE
        static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = StjEncoding.tuple1D (dec codec1)                                                                               <-> StjEncoding.tuple1E (enc codec1)
        static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = StjEncoding.tuple2D (dec codec1) (dec codec2)                                                                  <-> StjEncoding.tuple2E (enc codec1) (enc codec2)
        static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = StjEncoding.tuple3D (dec codec1) (dec codec2) (dec codec3)                                                     <-> StjEncoding.tuple3E (enc codec1) (enc codec2) (enc codec3)
        static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = StjEncoding.tuple4D (dec codec1) (dec codec2) (dec codec3) (dec codec4)                                        <-> StjEncoding.tuple4E (enc codec1) (enc codec2) (enc codec3) (enc codec4)
        static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = StjEncoding.tuple5D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5)                           <-> StjEncoding.tuple5E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5)
        static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = StjEncoding.tuple6D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6)              <-> StjEncoding.tuple6E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6)
        static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = StjEncoding.tuple7D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6) (dec codec7) <-> StjEncoding.tuple7E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6) (enc codec7)

        static member boolean  : Codec<JsonValue, bool>      =  StjEncoding.booleanD <-> StjEncoding.booleanE
        static member string         = StjEncoding.stringD         <-> StjEncoding.stringE
        static member dateTime       = StjEncoding.dateTimeD       <-> StjEncoding.dateTimeE
        static member dateTimeOffset = StjEncoding.dateTimeOffsetD <-> StjEncoding.dateTimeOffsetE
        static member timeSpan       = StjEncoding.timeSpanD       <-> StjEncoding.timeSpanE
        static member decimal        = StjEncoding.decimalD        <-> StjEncoding.decimalE
        static member float          = StjEncoding.floatD          <-> StjEncoding.floatE
        static member float32        = StjEncoding.float32D        <-> StjEncoding.float32E
        static member int            = StjEncoding.intD            <-> StjEncoding.intE
        static member uint32         = StjEncoding.uint32D         <-> StjEncoding.uint32E
        static member int64          = StjEncoding.int64D          <-> StjEncoding.int64E
        static member uint64         = StjEncoding.uint64D         <-> StjEncoding.uint64E
        static member int16          = StjEncoding.int16D          <-> StjEncoding.int16E
        static member uint16         = StjEncoding.uint16D         <-> StjEncoding.uint16E
        static member byte           = StjEncoding.byteD           <-> StjEncoding.byteE
        static member sbyte          = StjEncoding.sbyteD          <-> StjEncoding.sbyteE
        static member char           = StjEncoding.charD           <-> StjEncoding.charE
        static member guid           = StjEncoding.guidD           <-> StjEncoding.guidE


        interface IEncoding with
            member _.unit           = StjEncoding.toIRawCodec (StjEncoding.unitD <-> StjEncoding.unitE)
            member _.boolean        = StjEncoding.toIRawCodec StjEncoding.boolean
            member _.string         = StjEncoding.toIRawCodec StjEncoding.string
            member _.dateTime       = StjEncoding.toIRawCodec StjEncoding.dateTime
            member _.dateTimeOffset = StjEncoding.toIRawCodec StjEncoding.dateTimeOffset
            member _.timeSpan       = StjEncoding.toIRawCodec StjEncoding.timeSpan
            member _.decimal        = StjEncoding.toIRawCodec StjEncoding.decimal
            member _.float          = StjEncoding.toIRawCodec StjEncoding.float
            member _.float32        = StjEncoding.toIRawCodec StjEncoding.float32
            member _.int            = StjEncoding.toIRawCodec StjEncoding.int
            member _.uint32         = StjEncoding.toIRawCodec StjEncoding.uint32
            member _.int64          = StjEncoding.toIRawCodec StjEncoding.int64
            member _.uint64         = StjEncoding.toIRawCodec StjEncoding.uint64
            member _.int16          = StjEncoding.toIRawCodec StjEncoding.int16
            member _.uint16         = StjEncoding.toIRawCodec StjEncoding.uint16
            member _.byte           = StjEncoding.toIRawCodec StjEncoding.byte
            member _.sbyte          = StjEncoding.toIRawCodec StjEncoding.sbyte
            member _.char           = StjEncoding.toIRawCodec StjEncoding.char
            member _.guid           = StjEncoding.toIRawCodec StjEncoding.guid

            member _.result c1 c2     = StjEncoding.toIRawCodec (StjEncoding.result   (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2))
            member _.choice c1 c2     = StjEncoding.toIRawCodec (StjEncoding.choice   (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2))
            member _.choice3 c1 c2 c3 = StjEncoding.toIRawCodec (StjEncoding.choice3  (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3))
            member _.option c         = StjEncoding.toIRawCodec (StjEncoding.option   (StjEncoding.ofIRawCodec c))
            member _.nullable c       = StjEncoding.toIRawCodec (StjEncoding.nullable (StjEncoding.ofIRawCodec c))
            member _.array c          = StjEncoding.toIRawCodec (StjEncoding.array    (StjEncoding.ofIRawCodec c))
            member _.multiMap c       = StjEncoding.toIRawCodec (StjEncoding.multiMap (StjEncoding.ofIRawCodec c))

            member _.tuple1 c                    = StjEncoding.toIRawCodec (StjEncoding.tuple1 (StjEncoding.ofIRawCodec c))
            member _.tuple2 c1 c2                = StjEncoding.toIRawCodec (StjEncoding.tuple2 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2))
            member _.tuple3 c1 c2 c3             = StjEncoding.toIRawCodec (StjEncoding.tuple3 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3))
            member _.tuple4 c1 c2 c3 c4          = StjEncoding.toIRawCodec (StjEncoding.tuple4 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3) (StjEncoding.ofIRawCodec c4))
            member _.tuple5 c1 c2 c3 c4 c5       = StjEncoding.toIRawCodec (StjEncoding.tuple5 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3) (StjEncoding.ofIRawCodec c4) (StjEncoding.ofIRawCodec c5))
            member _.tuple6 c1 c2 c3 c4 c5 c6    = StjEncoding.toIRawCodec (StjEncoding.tuple6 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3) (StjEncoding.ofIRawCodec c4) (StjEncoding.ofIRawCodec c5) (StjEncoding.ofIRawCodec c6))
            member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = StjEncoding.toIRawCodec (StjEncoding.tuple7 (StjEncoding.ofIRawCodec c1) (StjEncoding.ofIRawCodec c2) (StjEncoding.ofIRawCodec c3) (StjEncoding.ofIRawCodec c4) (StjEncoding.ofIRawCodec c5) (StjEncoding.ofIRawCodec c6) (StjEncoding.ofIRawCodec c7))

            // Requires F# 5.0
            member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> (_: Codec<IEncoding, 'u>) : Codec<IEncoding, 't> = StjEncoding.toIRawCodec (StjEncoding.enumD <-> StjEncoding.enumE)

            member x.getCase =
                match x with
                | StjEncoding (JNull    ) -> "JNull"
                | StjEncoding (JBool   _) -> "JBool" 
                | StjEncoding (JNumber _) -> "JNumber"
                | StjEncoding (JString _) -> "JString"
                | StjEncoding (JArray  _) -> "JArray"
                | StjEncoding (JObject _) -> "JObject"



    ///////////////////////
    // Main entry points //
    ///////////////////////

    /// Get the json encoding representation of the value, using its default codec.
    let inline toJson (x: 'T) : StjEncoding = toEncoding<StjEncoding, 'T> x

    /// Attempts to decode the value from its json encoding representation, using its default codec.
    let inline ofJson (x: StjEncoding) : Result<'T, DecodeError> = ofEncoding x

    /// Get the json value representation of the value, using its default codec.
    let inline toJsonValue (x: 'T) : JsonValue = toEncoding<StjEncoding, 'T> x |> StjEncoding.Unwrap

    /// Attempts to decode the value from its json value representation, using its default codec.
    let inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (StjEncoding x)

    /// Get the json text representation of the value, using its default codec.
    let inline toJsonText (x: StjEncoding) = x |> toJson |> string

    /// Attempts to decode the value from its json text representation, using its default codec.
    let inline ofJsonText (x: string) = try Ok (StjEncoding.Parse x) with e -> Decode.Fail.parseError e x

    
    // Backwards compatibility functions
    module Operators =

        type JsonObject = Map<string, StjEncoding>

        let jobj (x: list<string * 'value>) : 'value =            
            let (Codec (_, enc)) = Codecs.multiMap (Ok <-> id)
            multiMap (x |> Seq.map System.Collections.Generic.KeyValuePair)
            |> enc

        let JString x = StjEncoding (JString x)

        let JObject x =
            (Codecs.multiMap (Ok <-> id)
            |> Codec.encode) x

        let (|JObject|_|) (x: StjEncoding) =
            (Codecs.multiMap (Ok <-> id)
            |> Codec.decode) x
            |> Option.ofResult

        let (|JNull|_|) (x: StjEncoding) =
            let (Codec (dec, _)) = Codecs.nullable (Ok <-> id)
            match dec x with
            | Ok x when Nullable.isNull x -> Some ()
            | _ -> None

        let (|JString|_|) (x: StjEncoding) =
            let (Codec (dec, _)) = Codecs.string
            dec x |> Option.ofResult

        /// A codec to encode a collection of property/values into a Json encoding and the other way around.
        let jsonObjToValueCodec : Codec<StjEncoding, MultiObj<StjEncoding>> = ((  function JObject (o: MultiMap<_,_>) -> Ok o | a -> Decode.Fail.objExpected a) <-> JObject)

        /// A codec to encode a Json value to a Json text and the other way around.
        let jsonValueToTextCodec = (fun x -> try Ok (StjEncoding.Parse x) with e -> Decode.Fail.parseError e x) <-> (fun (x: StjEncoding) -> string x)

        let inline parseJson (x: string) : ParseResult<'T> = Codec.decode jsonValueToTextCodec x >>= ofJson

        let inline jreq name getter = req name getter : Codec<MultiObj<StjEncoding>,_,_,_>
        let inline jopt name getter = opt name getter : Codec<MultiObj<StjEncoding>,_,_,_>
    
        let inline jreqWith codec name getter = optWith codec name getter : Codec<MultiObj<StjEncoding>,_,_,_>
        let inline joptWith codec name getter = optWith codec name getter : Codec<MultiObj<StjEncoding>,_,_,_>

        let inline jchoice (codecs: seq<Codec<MultiObj<StjEncoding>, MultiObj<StjEncoding>, 't1, 't2>>) =
            let head, tail = Seq.head codecs, Seq.tail codecs
            foldBack (<|>) tail head

        

        /// Gets a value from a Json object
        let jgetWith ofJson (o: MultiObj<StjEncoding>) key =
            match o.[key] with
            | value::_ -> ofJson value
            | _ -> Decode.Fail.propertyNotFound key (o |> MultiMap.mapValues (fun x -> x :> IEncoding))

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let jgetOptWith ofJson (o: MultiObj<StjEncoding>) key =
            match o.[key] with
            | JNull _::_ -> Ok None
            | value  ::_ -> ofJson value |> Result.map Some
            | _ -> Ok None

        /// Gets a value from a Json object
        let inline jget (o: MultiObj<StjEncoding>) key = jgetWith ofEncoding o key

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let inline jgetOpt (o: MultiObj<StjEncoding>) key = jgetOptWith ofEncoding o key

        /// Gets a value from a Json object
        let inline (.@) o key = jget o key

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let inline (.@?) o key = jgetOpt o key

        /// Creates a new Json key-value pair for a Json object
        let inline jpairWith toJson (key: string) value = key, toJson value

        /// Creates a new Json key-value pair for a Json object
        let inline jpair (key: string) value = jpairWith toEncoding key value


        /// Creates a new Json key-value pair for a Json object
        let inline (.=) key value = jpair key value

        let jsonObjectGetValues x = id x


        // Verbose syntax

        /// <summary>Initialize the field mappings.</summary>
        /// <param name="f">An object constructor as a curried function.</param>
        /// <returns>The resulting object codec.</returns>
        let inline withFields f : Codec<'s,'s,_,_> = result f //(fun _ -> Ok f) <-> (fun _ -> multiMap [])
        
        /// <summary>Appends a field mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline jfield    fieldName getter rest = rest <*> jreq fieldName (getter >> Some)
        
        /// <summary>Appends an optional field mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline jfieldOpt fieldName getter rest = rest <*> jopt fieldName getter

        /// <summary>Appends a field mapping to the codec.</summary>
        /// <param name="codec">The codec to be used.</param>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline jfieldWith codec fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _ -> 'Rest, _>) = rest <*> jreqWith codec fieldName (getter >> Some)

        /// <summary>Appends an optional field mapping to the codec.</summary>
        /// <param name="codec">The codec to be used.</param>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline jfieldOptWith codec fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _ -> 'Rest, _>) = rest <*> joptWith codec fieldName getter