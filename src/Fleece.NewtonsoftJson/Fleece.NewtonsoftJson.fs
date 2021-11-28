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
open Internals


type [<Struct>] Encoding = Encoding of JsonValue with

    override this.ToString () = let (Encoding x) = this in x.ToString ()
        
    static member Parse (x: string) = Encoding (JsonValue.Parse x)
        
    static member inline tryRead<'a> x =
        match x with
        | JNumber j -> 
            try
              Ok (j.ToObject<'a> ())
            with e -> Decode.Fail.invalidValue (Encoding j) (string e)
        | JString _ -> 
            try
                Ok (x.ToObject<'a> ())
            with e -> Decode.Fail.invalidValue (Encoding x) (string e)
        | js -> Decode.Fail.numExpected (Encoding js)

    /// Unwraps the JsonValue inside an IEncoding
    static member Unwrap (x: IEncoding) = x :?> Encoding |> fun (Encoding s) -> s

    /// Wraps a JsonValue inside an IEncoding
    static member Wrap x = Encoding x :> IEncoding

    static member toIEncoding (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((Encoding.Unwrap >> Ok) <-> Encoding.Wrap)
    static member ofIEncoding (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((Encoding.Wrap >> Ok) <-> Encoding.Unwrap)

    static member jsonObjectOfJson = 
        fun (o: JToken) ->
            match o.Type with
            | JTokenType.Object -> Ok (o :?> JObject)
            | _ -> Decode.Fail.objExpected (Encoding o)

    static member jsonOfJsonObject o = o :> JToken

    static member createTuple c t = function 
        | JArray a as x -> if length a <> c then Decode.Fail.count c (Encoding x) else t a
        | a -> Decode.Fail.arrExpected (Encoding a)

    
    //////////////
    // Decoders //
    //////////////

    static member resultD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Result<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Ok", a)]    -> a |> decoder1 |> Result.map Ok
            | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
            | _ -> Decode.Fail.invalidValue (Encoding jobj) ""
        | a -> Decode.Fail.objExpected (Encoding a)

    static member choiceD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
            | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
            | _ -> Decode.Fail.invalidValue (Encoding jobj) ""
        | a -> Decode.Fail.objExpected (Encoding a)

    static member choice3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
            | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
            | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
            | _ -> Decode.Fail.invalidValue (Encoding jobj) ""
        | a     -> Decode.Fail.objExpected (Encoding a)

    static member optionD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
        | JNull _ -> Ok None
        | x       -> Result.map Some (decoder x)

    static member nullableD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
        | JNull _ -> Ok (Nullable ())
        | x       -> Result.map Nullable (decoder x)

    static member arrayD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
        | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected (Encoding a)
        
    static member propListD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<PropertyList<'a>> = function
        | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toArray |> PropertyList)
        | a         -> Decode.Fail.objExpected (Encoding a)

    static member unitD : JsonValue -> ParseResult<unit> =
        Encoding.createTuple 0 (fun _ -> (Ok ()))

    static member tuple1D (decoder1: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Tuple<'a>> =
        Encoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

    static member tuple2D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> =
        Encoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

    static member tuple3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> =
        Encoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
    static member tuple4D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
        Encoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
    static member tuple5D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
        Encoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
    static member tuple6D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
        Encoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
    static member tuple7D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
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
        | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (Encoding x) s
        | a -> Decode.Fail.strExpected (Encoding a)

    static member booleanD x =
        match x with
        | JBool b -> Ok b
        | a -> Decode.Fail.boolExpected (Encoding a)

    static member stringD x =
        match x with
        | JString b -> Ok b
        | JNull     -> Ok null
        | a -> Decode.Fail.strExpected (Encoding a)

    static member charD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> Ok s.[0]
        | a -> Decode.Fail.strExpected (Encoding a)

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (Encoding x) s
        | a -> Decode.Fail.strExpected (Encoding a)

    static member dateTimeD x =
        match x with
        | JString null
        | JDate null -> Decode.Fail.nullString
        | JString s -> 
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (Encoding x) ""
        | JDate d -> Ok (d.Value<DateTime> ())
        | a -> Decode.Fail.strExpected (Encoding a)

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (Encoding x) ""
        | a -> Decode.Fail.strExpected (Encoding a)

    static member timeSpanD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JNumber _ as j -> Encoding.int64D j |> Result.map TimeSpan
        | a -> Decode.Fail.numExpected (Encoding a)


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
    static member propListE (encoder: _ -> JsonValue) (x: PropertyList<'a>) = x |> filter (fun (k, _) -> not (isNull k)) |> map encoder |> JObject

    static member tuple1E (encoder1: 'a -> JsonValue) (a: Tuple<_>) = JArray ([|encoder1 a.Item1|] |> Seq.toList)
    static member tuple2E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (a, b) = JArray ([|encoder1 a; encoder2 b|] |> Seq.toList)
    static member tuple3E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (a, b, c) = JArray ([|encoder1 a; encoder2 b; encoder3 c|] |> Seq.toList)
    static member tuple4E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (a, b, c, d) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d|] |> Seq.toList)
    static member tuple5E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (a, b, c, d, e) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e|] |> Seq.toList)
    static member tuple6E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (a, b, c, d, e, f) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f|] |> Seq.toList)
    static member tuple7E (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (encoder7: 'g -> JsonValue) (a, b, c, d, e, f, g) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f; encoder7 g|] |> Seq.toList)
    
    static member enumE (x: 't when 't: enum<_>) = JString (string x)
    static member unitE () = JArray []

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

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.resultD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.resultE (Codec.encode codec1) (Codec.encode codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.choiceD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.choiceE (Codec.encode codec1) (Codec.encode codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = Encoding.choice3D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) <-> Encoding.choice3E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3)
    static member option (codec: Codec<_,_>) = Encoding.optionD (Codec.decode codec) <-> Encoding.optionE (Codec.encode codec)
    static member nullable (codec: Codec<JsonValue, 't>) = Encoding.nullableD (Codec.decode codec) <-> Encoding.nullableE (Codec.encode codec) : Codec<JsonValue, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = Encoding.arrayD    (Codec.decode codec) <-> Encoding.arrayE    (Codec.encode codec)
    static member multiMap (codec: Codec<_,_>) = Encoding.propListD (Codec.decode codec) <-> Encoding.propListE (Codec.encode codec)

    static member unit = Encoding.unitD <-> Encoding.unitE
    static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = Encoding.tuple1D (Codec.decode codec1)                                                                                                                                     <-> Encoding.tuple1E (Codec.encode codec1)
    static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = Encoding.tuple2D (Codec.decode codec1) (Codec.decode codec2)                                                                                                               <-> Encoding.tuple2E (Codec.encode codec1) (Codec.encode codec2)
    static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = Encoding.tuple3D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3)                                                                                         <-> Encoding.tuple3E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3)
    static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = Encoding.tuple4D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4)                                                                   <-> Encoding.tuple4E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4)
    static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = Encoding.tuple5D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5)                                             <-> Encoding.tuple5E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5)
    static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = Encoding.tuple6D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5) (Codec.decode codec6)                       <-> Encoding.tuple6E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5) (Codec.encode codec6)
    static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = Encoding.tuple7D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) (Codec.decode codec4) (Codec.decode codec5) (Codec.decode codec6) (Codec.decode codec7) <-> Encoding.tuple7E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3) (Codec.encode codec4) (Codec.encode codec5) (Codec.encode codec6) (Codec.encode codec7)

    static member boolean        =  Encoding.booleanD       <-> Encoding.booleanE
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
        member _.propertyList c   = Encoding.toIEncoding (Encoding.multiMap (Encoding.ofIEncoding c))

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
            | Encoding (JNull    ) -> "JNull"
            | Encoding (JBool   _) -> "JBool" 
            | Encoding (JNumber _) -> "JNumber"
            | Encoding (JString _) -> "JString"
            | Encoding (JArray  _) -> "JArray"
            | Encoding (JObject _) -> "JObject"