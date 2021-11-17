namespace Fleece.SystemJson

open System
open System.Collections.Generic
open System.Json

module Internals =

   open FSharpPlus
   
   type JsonObject with
       member x.AsReadOnlyDictionary () = (x :> IDictionary<string, JsonValue>) |> Dict.toIReadOnlyDictionary
       static member GetValues (x: JsonObject) = x.AsReadOnlyDictionary ()

   let jsonObjectGetValues (x: JsonObject) = JsonObject.GetValues x

   type JsonHelpers () =
       static member create (x: decimal) = JsonPrimitive x :> JsonValue
       static member create (x: Double ) = JsonPrimitive x :> JsonValue
       static member create (x: Single ) = JsonPrimitive x :> JsonValue
       static member create (x: int    ) = JsonPrimitive x :> JsonValue
       static member create (x: uint32 ) = JsonPrimitive x :> JsonValue
       static member create (x: int64  ) = JsonPrimitive x :> JsonValue
       static member create (x: uint64 ) = JsonPrimitive x :> JsonValue
       static member create (x: int16  ) = JsonPrimitive x :> JsonValue
       static member create (x: uint16 ) = JsonPrimitive x :> JsonValue
       static member create (x: byte   ) = JsonPrimitive x :> JsonValue
       static member create (x: sbyte  ) = JsonPrimitive x :> JsonValue
       static member create (x: char   ) = JsonPrimitive (string x) :> JsonValue
       static member create (x: Guid   ) = JsonPrimitive (string x) :> JsonValue


   // pseudo-AST, wrapping JsonValue subtypes:

   let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonValue) =
       match o with
       | null -> JNull
       | :? JsonArray  as x -> JArray ((x :> JsonValue IList) |> IList.toIReadOnlyList)
       | :? JsonObject as x -> JObject (x.AsReadOnlyDictionary ())
       | :? JsonPrimitive as x ->
           match x.JsonType with
           | JsonType.Number  -> JNumber x
           | JsonType.Boolean -> JBool   (implicit x: bool)
           | JsonType.String  -> JString (implicit x: string)
           | _ -> failwithf "Invalid JsonType %A for primitive %A" x.JsonType x
       | _ -> failwithf "Invalid JsonValue %A" o

   let inline JArray (x: JsonValue IReadOnlyList) = JsonArray x :> JsonValue
   let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonObject x :> JsonValue
   let inline JBool (x: bool) = JsonPrimitive x :> JsonValue
   let JNull : JsonValue = null
   let inline JString (x: string) = if isNull x then JNull else JsonPrimitive x :> JsonValue
   let inline JNumber (x: decimal) = JsonPrimitive x :> JsonValue

   /// Creates a new Json object for serialization
   let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> readOnlyDict)


open System.Globalization
open FSharpPlus
open FSharpPlus.Data
open Fleece
open Fleece.Helpers
open Fleece.Operators
open Internals


type [<Struct>] SjEncoding = SjEncoding of JsonValue with

    override this.ToString () = let (SjEncoding x) = this in x.ToString ()
        
    static member Parse (x: string) = SjEncoding (JsonValue.Parse x)
        
    static member inline tryRead x =
        match x with
        | JNumber j ->
            try
                Ok (implicit j)
            with e -> Decode.Fail.invalidValue (SjEncoding j) (string e)
        | js -> Decode.Fail.numExpected (SjEncoding js)

    /// Unwraps the JsonValue inside an IEncoding
    static member Unwrap (x: IEncoding) = x :?> SjEncoding |> fun (SjEncoding s) -> s

    /// Wraps a JsonValue inside an IEncoding
    static member Wrap x = SjEncoding x :> IEncoding

    static member toIRawCodec (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((SjEncoding.Unwrap >> Ok) <-> SjEncoding.Wrap)
    static member ofIRawCodec (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((SjEncoding.Wrap >> Ok) <-> SjEncoding.Unwrap)

    static member inline jsonObjectOfJson =
        fun (o: JsonValue) ->
            match box o with
            | :? JsonObject as x -> Ok x
            | _ -> Decode.Fail.objExpected (SjEncoding o)

    static member jsonOfJsonObject (o: JsonObject) = o :> JsonValue

    static member createTuple c t = function 
        | JArray a as x -> if length a <> c then Decode.Fail.count c (SjEncoding x) else t a
        | a -> Decode.Fail.arrExpected (SjEncoding a)

    
    //////////////
    // Decoders //
    //////////////

    static member resultD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Result<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Ok", a)] -> a |> decoder1 |> Result.map Ok
            | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
            | _ -> Decode.Fail.invalidValue (SjEncoding jobj) ""
        | a -> Decode.Fail.objExpected (SjEncoding a)

    static member choiceD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
            | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
            | _ -> Decode.Fail.invalidValue (SjEncoding jobj) ""
        | a -> Decode.Fail.objExpected (SjEncoding a)

    static member choice3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
            | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
            | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
            | _ -> Decode.Fail.invalidValue (SjEncoding jobj) ""
        | a     -> Decode.Fail.objExpected (SjEncoding a)

    static member optionD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
        | JNull _ -> Ok None
        | x       -> Result.map Some (decoder x)

    static member nullableD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
        | JNull _ -> Ok (Nullable ())
        | x       -> Result.map Nullable (decoder x)

    static member arrayD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
        | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected (SjEncoding a)
        
    static member multiMapD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<MultiObj<'a>> = function
        | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toList |> List.map KeyValuePair |> multiMap)
        | a         -> Decode.Fail.objExpected (SjEncoding a)

    static member unitD : JsonValue -> ParseResult<unit> =
        SjEncoding.createTuple 0 (fun _ -> (Ok ()))

    static member tuple1D (decoder1: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Tuple<'a>> =
        SjEncoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

    static member tuple2D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> =
        SjEncoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

    static member tuple3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> =
        SjEncoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
    static member tuple4D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
        SjEncoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
    static member tuple5D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
        SjEncoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
    static member tuple6D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
        SjEncoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
    static member tuple7D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
        SjEncoding.createTuple 7 (fun a -> Result.map (fun a b c d e f g -> (a, b, c, d, e, f, g)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6])

    static member decimalD x = SjEncoding.tryRead<decimal> x
    static member int16D   x = SjEncoding.tryRead<int16>   x
    static member intD     x = SjEncoding.tryRead<int>     x
    static member int64D   x = SjEncoding.tryRead<int64>   x
    static member uint16D  x = SjEncoding.tryRead<uint16>  x
    static member uint32D  x = SjEncoding.tryRead<uint32>  x
    static member uint64D  x = SjEncoding.tryRead<uint64>  x
    static member byteD    x = SjEncoding.tryRead<byte>    x
    static member sbyteD   x = SjEncoding.tryRead<sbyte>   x
    static member floatD   x = SjEncoding.tryRead<double>  x
    static member float32D x = SjEncoding.tryRead<single>  x

    static member enumD x : Result< 't, _> when 't: enum<_> =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (SjEncoding x) s
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member booleanD x =
        match x with
        | JBool b -> Ok b
        | a -> Decode.Fail.boolExpected (SjEncoding a)

    static member stringD x =
        match x with
        | JString b -> Ok b
        | JNull     -> Ok null
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member charD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> Ok s.[0]
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (SjEncoding x) s
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member dateTimeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (SjEncoding x) ""
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (SjEncoding x) ""
        | a -> Decode.Fail.strExpected (SjEncoding a)

    static member timeSpanD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JNumber _ as j -> SjEncoding.int64D j |> Result.map TimeSpan
        | a -> Decode.Fail.numExpected (SjEncoding a)


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

    static member nullableE (encoder: _ -> JsonValue) (x: Nullable<'a>) = if x.HasValue then encoder x.Value else JNull
    
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

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = SjEncoding.resultD (dec codec1) (dec codec2) <-> SjEncoding.resultE (enc codec1) (enc codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = SjEncoding.choiceD (dec codec1) (dec codec2) <-> SjEncoding.choiceE (enc codec1) (enc codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = SjEncoding.choice3D (dec codec1) (dec codec2) (dec codec3) <-> SjEncoding.choice3E (enc codec1) (enc codec2) (enc codec3)
    static member option (codec: Codec<_,_>) = SjEncoding.optionD (dec codec) <-> SjEncoding.optionE (enc codec)
    static member nullable (codec: Codec<JsonValue, 't>) = SjEncoding.nullableD (dec codec) <-> SjEncoding.nullableE (enc codec) : Codec<JsonValue, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = SjEncoding.arrayD  (dec codec) <-> SjEncoding.arrayE    (enc codec)
    static member multiMap (codec: Codec<_,_>) = SjEncoding.multiMapD (dec codec) <-> SjEncoding.multiMapE (enc codec)

    static member unit () = SjEncoding.unitD <-> SjEncoding.unitE
    static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = SjEncoding.tuple1D (dec codec1)                                                                               <-> SjEncoding.tuple1E (enc codec1)
    static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = SjEncoding.tuple2D (dec codec1) (dec codec2)                                                                  <-> SjEncoding.tuple2E (enc codec1) (enc codec2)
    static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = SjEncoding.tuple3D (dec codec1) (dec codec2) (dec codec3)                                                     <-> SjEncoding.tuple3E (enc codec1) (enc codec2) (enc codec3)
    static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = SjEncoding.tuple4D (dec codec1) (dec codec2) (dec codec3) (dec codec4)                                        <-> SjEncoding.tuple4E (enc codec1) (enc codec2) (enc codec3) (enc codec4)
    static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = SjEncoding.tuple5D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5)                           <-> SjEncoding.tuple5E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5)
    static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = SjEncoding.tuple6D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6)              <-> SjEncoding.tuple6E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6)
    static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = SjEncoding.tuple7D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6) (dec codec7) <-> SjEncoding.tuple7E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6) (enc codec7)

    static member boolean  : Codec<JsonValue, bool>      =  SjEncoding.booleanD <-> SjEncoding.booleanE
    static member string         = SjEncoding.stringD         <-> SjEncoding.stringE
    static member dateTime       = SjEncoding.dateTimeD       <-> SjEncoding.dateTimeE
    static member dateTimeOffset = SjEncoding.dateTimeOffsetD <-> SjEncoding.dateTimeOffsetE
    static member timeSpan       = SjEncoding.timeSpanD       <-> SjEncoding.timeSpanE
    static member decimal        = SjEncoding.decimalD        <-> SjEncoding.decimalE
    static member float          = SjEncoding.floatD          <-> SjEncoding.floatE
    static member float32        = SjEncoding.float32D        <-> SjEncoding.float32E
    static member int            = SjEncoding.intD            <-> SjEncoding.intE
    static member uint32         = SjEncoding.uint32D         <-> SjEncoding.uint32E
    static member int64          = SjEncoding.int64D          <-> SjEncoding.int64E
    static member uint64         = SjEncoding.uint64D         <-> SjEncoding.uint64E
    static member int16          = SjEncoding.int16D          <-> SjEncoding.int16E
    static member uint16         = SjEncoding.uint16D         <-> SjEncoding.uint16E
    static member byte           = SjEncoding.byteD           <-> SjEncoding.byteE
    static member sbyte          = SjEncoding.sbyteD          <-> SjEncoding.sbyteE
    static member char           = SjEncoding.charD           <-> SjEncoding.charE
    static member guid           = SjEncoding.guidD           <-> SjEncoding.guidE


    interface IEncoding with
        member _.unit           = SjEncoding.toIRawCodec (SjEncoding.unitD <-> SjEncoding.unitE)
        member _.boolean        = SjEncoding.toIRawCodec SjEncoding.boolean
        member _.string         = SjEncoding.toIRawCodec SjEncoding.string
        member _.dateTime       = SjEncoding.toIRawCodec SjEncoding.dateTime
        member _.dateTimeOffset = SjEncoding.toIRawCodec SjEncoding.dateTimeOffset
        member _.timeSpan       = SjEncoding.toIRawCodec SjEncoding.timeSpan
        member _.decimal        = SjEncoding.toIRawCodec SjEncoding.decimal
        member _.float          = SjEncoding.toIRawCodec SjEncoding.float
        member _.float32        = SjEncoding.toIRawCodec SjEncoding.float32
        member _.int            = SjEncoding.toIRawCodec SjEncoding.int
        member _.uint32         = SjEncoding.toIRawCodec SjEncoding.uint32
        member _.int64          = SjEncoding.toIRawCodec SjEncoding.int64
        member _.uint64         = SjEncoding.toIRawCodec SjEncoding.uint64
        member _.int16          = SjEncoding.toIRawCodec SjEncoding.int16
        member _.uint16         = SjEncoding.toIRawCodec SjEncoding.uint16
        member _.byte           = SjEncoding.toIRawCodec SjEncoding.byte
        member _.sbyte          = SjEncoding.toIRawCodec SjEncoding.sbyte
        member _.char           = SjEncoding.toIRawCodec SjEncoding.char
        member _.guid           = SjEncoding.toIRawCodec SjEncoding.guid

        member _.result c1 c2     = SjEncoding.toIRawCodec (SjEncoding.result   (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2))
        member _.choice c1 c2     = SjEncoding.toIRawCodec (SjEncoding.choice   (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2))
        member _.choice3 c1 c2 c3 = SjEncoding.toIRawCodec (SjEncoding.choice3  (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3))
        member _.option c         = SjEncoding.toIRawCodec (SjEncoding.option   (SjEncoding.ofIRawCodec c))
        member _.nullable c       = SjEncoding.toIRawCodec (SjEncoding.nullable (SjEncoding.ofIRawCodec c))
        member _.array c          = SjEncoding.toIRawCodec (SjEncoding.array    (SjEncoding.ofIRawCodec c))
        member _.multiMap c       = SjEncoding.toIRawCodec (SjEncoding.multiMap (SjEncoding.ofIRawCodec c))

        member _.tuple1 c                    = SjEncoding.toIRawCodec (SjEncoding.tuple1 (SjEncoding.ofIRawCodec c))
        member _.tuple2 c1 c2                = SjEncoding.toIRawCodec (SjEncoding.tuple2 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2))
        member _.tuple3 c1 c2 c3             = SjEncoding.toIRawCodec (SjEncoding.tuple3 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3))
        member _.tuple4 c1 c2 c3 c4          = SjEncoding.toIRawCodec (SjEncoding.tuple4 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3) (SjEncoding.ofIRawCodec c4))
        member _.tuple5 c1 c2 c3 c4 c5       = SjEncoding.toIRawCodec (SjEncoding.tuple5 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3) (SjEncoding.ofIRawCodec c4) (SjEncoding.ofIRawCodec c5))
        member _.tuple6 c1 c2 c3 c4 c5 c6    = SjEncoding.toIRawCodec (SjEncoding.tuple6 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3) (SjEncoding.ofIRawCodec c4) (SjEncoding.ofIRawCodec c5) (SjEncoding.ofIRawCodec c6))
        member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = SjEncoding.toIRawCodec (SjEncoding.tuple7 (SjEncoding.ofIRawCodec c1) (SjEncoding.ofIRawCodec c2) (SjEncoding.ofIRawCodec c3) (SjEncoding.ofIRawCodec c4) (SjEncoding.ofIRawCodec c5) (SjEncoding.ofIRawCodec c6) (SjEncoding.ofIRawCodec c7))

        // Requires F# 5.0
        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> (_: Codec<IEncoding, 'u>) : Codec<IEncoding, 't> = SjEncoding.toIRawCodec (SjEncoding.enumD <-> SjEncoding.enumE)

        member x.getCase =
            match x with
            | SjEncoding (JNull    ) -> "JNull"
            | SjEncoding (JBool   _) -> "JBool" 
            | SjEncoding (JNumber _) -> "JNumber"
            | SjEncoding (JString _) -> "JString"
            | SjEncoding (JArray  _) -> "JArray"
            | SjEncoding (JObject _) -> "JObject"


[<AutoOpen>]
type Operators =

    ///////////////////////
    // Main entry points //
    ///////////////////////

    /// Get the json encoding representation of the value, using its default codec.
    static member inline toJson (x: 'T) : SjEncoding = toEncoding<SjEncoding, 'T> x

    /// Attempts to decode the value from its json encoding representation, using its default codec.
    static member inline ofJson (x: SjEncoding) : Result<'T, DecodeError> = ofEncoding x

    /// Get the json value representation of the value, using its default codec.
    static member inline toJsonValue (x: 'T) : JsonValue = toEncoding<SjEncoding, 'T> x |> SjEncoding.Unwrap

    /// Attempts to decode the value from its json value representation, using its default codec.
    static member inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (SjEncoding x)

    /// Get the json text representation of the value, using its default codec.
    static member inline toJsonText (x: 'T) = x |> toJson |> string
    
    /// Attempts to decode the value from its json text representation, using its default codec.
    static member inline ofJsonText (x: string) : Result<'T, DecodeError> = try (SjEncoding.Parse x |> ofEncoding) with e -> Decode.Fail.parseError e x