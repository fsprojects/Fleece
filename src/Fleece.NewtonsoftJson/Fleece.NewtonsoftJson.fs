namespace Fleece.Newtonsoft

open System
open System.Collections.Generic
open Newtonsoft.Json.Linq

type JsonValue = JToken

module Internals =

   open FSharpPlus
   
   type JObject with
       member x.AsReadOnlyDictionary () = (x.Properties () |> Seq.map (fun p -> (p.Name, p.Value)) |> dict) |> Dict.toIReadOnlyDictionary
       static member GetValues (x: JObject) = x.AsReadOnlyDictionary ()

   let jsonObjectGetValues (x : JObject) = JObject.GetValues x

   type JsonObject = JObject
   
   type JsonHelpers () =
       static member create (x: decimal)  = JValue          x  :> JToken
       static member create (x: Double )  = JValue          x  :> JToken
       static member create (x: Single )  = JValue          x  :> JToken
       static member create (x: int    )  = JValue          x  :> JToken
       static member create (x: uint32 )  = JValue          x  :> JToken
       static member create (x: int64  )  = JValue          x  :> JToken
       static member create (x: uint64 )  = JValue          x  :> JToken
       static member create (x: int16  )  = JValue          x  :> JToken
       static member create (x: uint16 )  = JValue          x  :> JToken
       static member create (x: byte   )  = JValue          x  :> JToken
       static member create (x: sbyte  )  = JValue          x  :> JToken
       static member create (x: char   )  = JValue (string  x) :> JToken
       static member create (x: Guid   )  = JValue (string  x) :> JToken
       static member create (x: DateTime) = JValue           x :> JToken


   // FSharp.Newtonsoft.Json AST adapter

   let (|JArray|JObject|JNumber|JBool|JString|JNull|JDate|) (o: JToken) =
       match o.Type with
       | JTokenType.Null    -> JNull
       | JTokenType.Array   -> JArray ((o :?> JArray) |> IList.toIReadOnlyList)
       | JTokenType.Object  -> JObject (jsonObjectGetValues (o :?> JObject))
       | JTokenType.Integer -> JNumber  o
       | JTokenType.Float   -> JNumber  o
       | JTokenType.Boolean -> JBool   (o.ToObject () : bool)
       | JTokenType.String  -> JString (o.ToObject () : string)
       | JTokenType.Date    -> JDate    o
       | t                  -> failwithf "Invalid JTokenType %A" t
   
   let dictAsProps (x: IReadOnlyDictionary<string, JToken>) = x |> Seq.map (|KeyValue|) |> Array.ofSeq

   let inline JArray (x: JToken IReadOnlyList) = JArray (x |> Array.ofSeq) :> JToken
   let inline JObject (x: IReadOnlyDictionary<string, JToken>) =
       let o = JObject ()
       for kv in x do
           o.Add (kv.Key, kv.Value)
       o :> JToken
   let inline JBool (x: bool) = JValue x :> JToken
   let inline JNumber (x: decimal) = JValue x :> JToken
   let JNull = JValue.CreateNull () :> JToken
   let inline JString (x: string) = if isNull x then JNull else JValue x :> JToken
   let inline JDate (x: DateTime) = JValue x :> JToken

   /// Creates a new Json object for serialization
   let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> Map.ofSeq)


open System.Globalization
open FSharpPlus
open FSharpPlus.Data
open Fleece
open Fleece.Helpers
open Fleece.Operators
open Internals


type [<Struct>] NsjEncoding = NsjEncoding of JsonValue with

    override this.ToString () = let (NsjEncoding x) = this in x.ToString ()
        
    static member Parse (x: string) = NsjEncoding (JsonValue.Parse x)
        
    static member inline tryRead<'a> x =
        match x with
        | JNumber j -> 
            try
              Ok (j.ToObject<'a> ())
            with
            | e -> Decode.Fail.invalidValue (NsjEncoding j) (string e)
        | JString _ -> 
            try
                Ok (x.ToObject<'a> ())
            with
            | e -> Decode.Fail.invalidValue (NsjEncoding x) (string e)
        | js -> Decode.Fail.numExpected (NsjEncoding js)

    /// Unwraps the JsonValue inside an IEncoding
    static member Unwrap (x: IEncoding) = x :?> NsjEncoding |> fun (NsjEncoding s) -> s

    /// Wraps a JsonValue inside an IEncoding
    static member Wrap x = NsjEncoding x :> IEncoding

    static member toIRawCodec (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((NsjEncoding.Unwrap >> Ok) <-> NsjEncoding.Wrap)
    static member ofIRawCodec (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((NsjEncoding.Wrap >> Ok) <-> NsjEncoding.Unwrap)

    static member jsonObjectOfJson = 
        fun (o: JToken) ->
            match o.Type with
            | JTokenType.Object -> Ok (o :?> JObject)
            | _ -> Decode.Fail.objExpected (NsjEncoding o)

    static member jsonOfJsonObject o = o :> JToken

    static member createTuple c t = function 
        | JArray a as x -> if length a <> c then Decode.Fail.count c (NsjEncoding x) else t a
        | a -> Decode.Fail.arrExpected (NsjEncoding a)

    
    //////////////
    // Decoders //
    //////////////

    static member resultD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Result<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Ok", a)] -> a |> decoder1 |> Result.map Ok
            | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
            | _ -> Decode.Fail.invalidValue (NsjEncoding jobj) ""
        | a -> Decode.Fail.objExpected (NsjEncoding a)

    static member choiceD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
            | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
            | _ -> Decode.Fail.invalidValue (NsjEncoding jobj) ""
        | a -> Decode.Fail.objExpected (NsjEncoding a)

    static member choice3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
            | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
            | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
            | _ -> Decode.Fail.invalidValue (NsjEncoding jobj) ""
        | a     -> Decode.Fail.objExpected (NsjEncoding a)

    static member optionD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
        | JNull _ -> Ok None
        | x       -> Result.map Some (decoder x)

    static member nullableD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
        | JNull _ -> Ok (Nullable ())
        | x       -> Result.map Nullable (decoder x)

    static member arrayD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
        | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected (NsjEncoding a)
        
    static member multiMapD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<MultiObj<'a>> = function
        | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toList |> List.map KeyValuePair |> multiMap)
        | a         -> Decode.Fail.objExpected (NsjEncoding a)

    static member unitD : JsonValue -> ParseResult<unit> =
        NsjEncoding.createTuple 0 (fun _ -> (Ok ()))

    static member tuple1D (decoder1: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Tuple<'a>> =
        NsjEncoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

    static member tuple2D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> =
        NsjEncoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

    static member tuple3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> =
        NsjEncoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
    static member tuple4D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
        NsjEncoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
    static member tuple5D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
        NsjEncoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
    static member tuple6D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
        NsjEncoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
    static member tuple7D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
        NsjEncoding.createTuple 7 (fun a -> Result.map (fun a b c d e f g -> (a, b, c, d, e, f, g)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6])

    static member decimalD x = NsjEncoding.tryRead<decimal> x
    static member int16D   x = NsjEncoding.tryRead<int16>   x
    static member intD     x = NsjEncoding.tryRead<int>     x
    static member int64D   x = NsjEncoding.tryRead<int64>   x
    static member uint16D  x = NsjEncoding.tryRead<uint16>  x
    static member uint32D  x = NsjEncoding.tryRead<uint32>  x
    static member uint64D  x = NsjEncoding.tryRead<uint64>  x
    static member byteD    x = NsjEncoding.tryRead<byte>    x
    static member sbyteD   x = NsjEncoding.tryRead<sbyte>   x
    static member floatD   x = NsjEncoding.tryRead<double>  x
    static member float32D x = NsjEncoding.tryRead<single>  x

    static member enumD x : Result< 't, _> when 't: enum<_> =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (NsjEncoding x) s
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member booleanD x =
        match x with
        | JBool b -> Ok b
        | a -> Decode.Fail.boolExpected (NsjEncoding a)

    static member stringD x =
        match x with
        | JString b -> Ok b
        | JNull     -> Ok null
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member charD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> Ok s.[0]
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (NsjEncoding x) s
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member dateTimeD x =
        match x with
        | JString null
        | JDate null -> Decode.Fail.nullString
        | JString s -> 
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (NsjEncoding x) ""
        | JDate d    ->
            Ok <| d.Value<DateTime>()
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (NsjEncoding x) ""
        | a -> Decode.Fail.strExpected (NsjEncoding a)

    static member timeSpanD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JNumber _ as j -> NsjEncoding.int64D j |> Result.map TimeSpan
        | a -> Decode.Fail.numExpected (NsjEncoding a)


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

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = NsjEncoding.resultD (dec codec1) (dec codec2) <-> NsjEncoding.resultE (enc codec1) (enc codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = NsjEncoding.choiceD (dec codec1) (dec codec2) <-> NsjEncoding.choiceE (enc codec1) (enc codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = NsjEncoding.choice3D (dec codec1) (dec codec2) (dec codec3) <-> NsjEncoding.choice3E (enc codec1) (enc codec2) (enc codec3)
    static member option (codec: Codec<_,_>) = NsjEncoding.optionD (dec codec) <-> NsjEncoding.optionE (enc codec)
    static member nullable (codec: Codec<JsonValue, 't>) = NsjEncoding.nullableD (dec codec) <-> NsjEncoding.nullableE (enc codec) : Codec<JsonValue, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = NsjEncoding.arrayD  (dec codec) <-> NsjEncoding.arrayE    (enc codec)
    static member multiMap (codec: Codec<_,_>) = NsjEncoding.multiMapD (dec codec) <-> NsjEncoding.multiMapE (enc codec)

    static member unit () = NsjEncoding.unitD <-> NsjEncoding.unitE
    static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = NsjEncoding.tuple1D (dec codec1)                                                                               <-> NsjEncoding.tuple1E (enc codec1)
    static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = NsjEncoding.tuple2D (dec codec1) (dec codec2)                                                                  <-> NsjEncoding.tuple2E (enc codec1) (enc codec2)
    static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = NsjEncoding.tuple3D (dec codec1) (dec codec2) (dec codec3)                                                     <-> NsjEncoding.tuple3E (enc codec1) (enc codec2) (enc codec3)
    static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = NsjEncoding.tuple4D (dec codec1) (dec codec2) (dec codec3) (dec codec4)                                        <-> NsjEncoding.tuple4E (enc codec1) (enc codec2) (enc codec3) (enc codec4)
    static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = NsjEncoding.tuple5D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5)                           <-> NsjEncoding.tuple5E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5)
    static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = NsjEncoding.tuple6D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6)              <-> NsjEncoding.tuple6E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6)
    static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = NsjEncoding.tuple7D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6) (dec codec7) <-> NsjEncoding.tuple7E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6) (enc codec7)

    static member boolean  : Codec<JsonValue, bool>      =  NsjEncoding.booleanD <-> NsjEncoding.booleanE
    static member string         = NsjEncoding.stringD         <-> NsjEncoding.stringE
    static member dateTime       = NsjEncoding.dateTimeD       <-> NsjEncoding.dateTimeE
    static member dateTimeOffset = NsjEncoding.dateTimeOffsetD <-> NsjEncoding.dateTimeOffsetE
    static member timeSpan       = NsjEncoding.timeSpanD       <-> NsjEncoding.timeSpanE
    static member decimal        = NsjEncoding.decimalD        <-> NsjEncoding.decimalE
    static member float          = NsjEncoding.floatD          <-> NsjEncoding.floatE
    static member float32        = NsjEncoding.float32D        <-> NsjEncoding.float32E
    static member int            = NsjEncoding.intD            <-> NsjEncoding.intE
    static member uint32         = NsjEncoding.uint32D         <-> NsjEncoding.uint32E
    static member int64          = NsjEncoding.int64D          <-> NsjEncoding.int64E
    static member uint64         = NsjEncoding.uint64D         <-> NsjEncoding.uint64E
    static member int16          = NsjEncoding.int16D          <-> NsjEncoding.int16E
    static member uint16         = NsjEncoding.uint16D         <-> NsjEncoding.uint16E
    static member byte           = NsjEncoding.byteD           <-> NsjEncoding.byteE
    static member sbyte          = NsjEncoding.sbyteD          <-> NsjEncoding.sbyteE
    static member char           = NsjEncoding.charD           <-> NsjEncoding.charE
    static member guid           = NsjEncoding.guidD           <-> NsjEncoding.guidE


    interface IEncoding with
        member _.unit           = NsjEncoding.toIRawCodec (NsjEncoding.unitD <-> NsjEncoding.unitE)
        member _.boolean        = NsjEncoding.toIRawCodec NsjEncoding.boolean
        member _.string         = NsjEncoding.toIRawCodec NsjEncoding.string
        member _.dateTime       = NsjEncoding.toIRawCodec NsjEncoding.dateTime
        member _.dateTimeOffset = NsjEncoding.toIRawCodec NsjEncoding.dateTimeOffset
        member _.timeSpan       = NsjEncoding.toIRawCodec NsjEncoding.timeSpan
        member _.decimal        = NsjEncoding.toIRawCodec NsjEncoding.decimal
        member _.float          = NsjEncoding.toIRawCodec NsjEncoding.float
        member _.float32        = NsjEncoding.toIRawCodec NsjEncoding.float32
        member _.int            = NsjEncoding.toIRawCodec NsjEncoding.int
        member _.uint32         = NsjEncoding.toIRawCodec NsjEncoding.uint32
        member _.int64          = NsjEncoding.toIRawCodec NsjEncoding.int64
        member _.uint64         = NsjEncoding.toIRawCodec NsjEncoding.uint64
        member _.int16          = NsjEncoding.toIRawCodec NsjEncoding.int16
        member _.uint16         = NsjEncoding.toIRawCodec NsjEncoding.uint16
        member _.byte           = NsjEncoding.toIRawCodec NsjEncoding.byte
        member _.sbyte          = NsjEncoding.toIRawCodec NsjEncoding.sbyte
        member _.char           = NsjEncoding.toIRawCodec NsjEncoding.char
        member _.guid           = NsjEncoding.toIRawCodec NsjEncoding.guid

        member _.result c1 c2     = NsjEncoding.toIRawCodec (NsjEncoding.result   (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2))
        member _.choice c1 c2     = NsjEncoding.toIRawCodec (NsjEncoding.choice   (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2))
        member _.choice3 c1 c2 c3 = NsjEncoding.toIRawCodec (NsjEncoding.choice3  (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3))
        member _.option c         = NsjEncoding.toIRawCodec (NsjEncoding.option   (NsjEncoding.ofIRawCodec c))
        member _.nullable c       = NsjEncoding.toIRawCodec (NsjEncoding.nullable (NsjEncoding.ofIRawCodec c))
        member _.array c          = NsjEncoding.toIRawCodec (NsjEncoding.array    (NsjEncoding.ofIRawCodec c))
        member _.multiMap c       = NsjEncoding.toIRawCodec (NsjEncoding.multiMap (NsjEncoding.ofIRawCodec c))

        member _.tuple1 c                    = NsjEncoding.toIRawCodec (NsjEncoding.tuple1 (NsjEncoding.ofIRawCodec c))
        member _.tuple2 c1 c2                = NsjEncoding.toIRawCodec (NsjEncoding.tuple2 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2))
        member _.tuple3 c1 c2 c3             = NsjEncoding.toIRawCodec (NsjEncoding.tuple3 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3))
        member _.tuple4 c1 c2 c3 c4          = NsjEncoding.toIRawCodec (NsjEncoding.tuple4 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3) (NsjEncoding.ofIRawCodec c4))
        member _.tuple5 c1 c2 c3 c4 c5       = NsjEncoding.toIRawCodec (NsjEncoding.tuple5 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3) (NsjEncoding.ofIRawCodec c4) (NsjEncoding.ofIRawCodec c5))
        member _.tuple6 c1 c2 c3 c4 c5 c6    = NsjEncoding.toIRawCodec (NsjEncoding.tuple6 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3) (NsjEncoding.ofIRawCodec c4) (NsjEncoding.ofIRawCodec c5) (NsjEncoding.ofIRawCodec c6))
        member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = NsjEncoding.toIRawCodec (NsjEncoding.tuple7 (NsjEncoding.ofIRawCodec c1) (NsjEncoding.ofIRawCodec c2) (NsjEncoding.ofIRawCodec c3) (NsjEncoding.ofIRawCodec c4) (NsjEncoding.ofIRawCodec c5) (NsjEncoding.ofIRawCodec c6) (NsjEncoding.ofIRawCodec c7))

        // Requires F# 5.0
        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> (_: Codec<IEncoding, 'u>) : Codec<IEncoding, 't> = NsjEncoding.toIRawCodec (NsjEncoding.enumD <-> NsjEncoding.enumE)

        member x.getCase =
            match x with
            | NsjEncoding (JNull    ) -> "JNull"
            | NsjEncoding (JBool   _) -> "JBool" 
            | NsjEncoding (JNumber _) -> "JNumber"
            | NsjEncoding (JString _) -> "JString"
            | NsjEncoding (JArray  _) -> "JArray"
            | NsjEncoding (JObject _) -> "JObject"


[<AutoOpen>]
type Operators =

    ///////////////////////
    // Main entry points //
    ///////////////////////

    /// Get the json encoding representation of the value, using its default codec.
    static member inline toJson (x: 'T) : NsjEncoding = toEncoding<NsjEncoding, 'T> x

    /// Attempts to decode the value from its json encoding representation, using its default codec.
    static member inline ofJson (x: NsjEncoding) : Result<'T, DecodeError> = ofEncoding x

    /// Get the json value representation of the value, using its default codec.
    static member inline toJsonValue (x: 'T) : JsonValue = toEncoding<NsjEncoding, 'T> x |> NsjEncoding.Unwrap

    /// Attempts to decode the value from its json value representation, using its default codec.
    static member inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (NsjEncoding x)

    /// Get the json text representation of the value, using its default codec.
    static member inline toJsonText (x: 'T) = x |> toJson |> string

    /// Attempts to decode the value from its json text representation, using its default codec.
    static member inline ofJsonText (x: string) : Result<'T, DecodeError> = try (NsjEncoding.Parse x |> ofEncoding) with e -> Decode.Fail.parseError e x