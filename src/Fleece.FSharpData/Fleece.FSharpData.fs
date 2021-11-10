namespace Fleece.FSharpData

open System
open System.Collections.Generic
open FSharp.Data

module Internals =

    open FSharpPlus

    type JsonHelpers () =
        static member create (x: decimal) : JsonValue = JsonValue.Number          x
        static member create (x: Double ) : JsonValue = JsonValue.Float           x
        static member create (x: Single ) : JsonValue = JsonValue.Float  (float   x)
        static member create (x: int    ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: bool   ) : JsonValue = JsonValue.Boolean         x
        static member create (x: uint32 ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: int64  ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: uint64 ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: int16  ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: uint16 ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: byte   ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: sbyte  ) : JsonValue = JsonValue.Number (decimal x)
        static member create (x: char   ) : JsonValue = JsonValue.String (string  x)
        static member create (x: Guid   ) : JsonValue = JsonValue.String (string  x)


    type JsonObject (properties: (string * JsonValue) []) =
        let properties = properties
        member __.Properties = properties
        with
            interface System.Collections.IEnumerable with
                member __.GetEnumerator () = (properties |> Seq.map KeyValuePair).GetEnumerator () :> System.Collections.IEnumerator
        
            interface IEnumerable<KeyValuePair<string, JsonValue>> with
                member __.GetEnumerator () = (properties |> Seq.map KeyValuePair).GetEnumerator ()
        
            interface IReadOnlyCollection<KeyValuePair<string,JsonValue>> with
                member __.Count = properties.Length
        
            interface IReadOnlyDictionary<string, JsonValue> with
                member __.Keys = properties |> Seq.map fst
                member __.Values = properties |> Seq.map snd
                member __.Item with get (key: string) = properties |> Array.find (fun (k, _) -> k = key) |> snd
                member __.ContainsKey (key: string) = properties |> Array.exists (fun (k, _) -> k = key)
                member __.TryGetValue (key: string, value:byref<JsonValue>) =
                    match properties |> Array.tryFindIndex (fun (k, _) -> k = key) with
                    | Some i ->
                        value <- snd properties.[i]
                        true
                    | None -> false
        
    let jsonObjectGetValues (x: JsonObject) = x :> IReadOnlyDictionary<string, JsonValue>


    // FSharp.Data.JsonValue AST adapter
    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonValue) =
        match o with
        | JsonValue.Null          -> JNull
        | JsonValue.Array els     -> JArray (IList.toIReadOnlyList els)
        | JsonValue.Record props  -> JObject (jsonObjectGetValues (JsonObject props))
        | JsonValue.Number _ as x -> JNumber x
        | JsonValue.Float _ as x  -> JNumber x
        | JsonValue.Boolean x     -> JBool x
        | JsonValue.String x      -> JString x
       
    let dictAsJsonObject (x: IReadOnlyDictionary<string, JsonValue>) =
        match x with
        | :? JsonObject as x' -> x'
        | _ -> x |> Seq.map (|KeyValue|) |> Array.ofSeq |> JsonObject
   
    let dictAsProps (x: IReadOnlyDictionary<string, JsonValue>) =
        match x with
        | :? JsonObject as x' -> x'.Properties
        | _ -> x |> Seq.map (|KeyValue|) |> Array.ofSeq
   
    let inline JArray (x: JsonValue IReadOnlyList) = JsonValue.Array (x |> Array.ofSeq)
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonValue.Record (dictAsProps x)
    let inline JBool (x: bool) = JsonValue.Boolean x
    let inline JNumber (x: decimal) = JsonValue.Number x
    let JNull : JsonValue = JsonValue.Null
    let inline JString (x: string) = if isNull x then JsonValue.Null else JsonValue.String x

    /// Creates a new Json object for serialization
    let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> readOnlyDict)


open System.Globalization
open FSharpPlus
open FSharpPlus.Data
open Fleece
open Fleece.Helpers
open Fleece.Operators
open Internals


type [<Struct>] FdEncoding = FdEncoding of JsonValue with

    override this.ToString () = let (FdEncoding x) = this in x.ToString ()
        
    static member Parse (x: string) = FdEncoding (JsonValue.Parse x)
        
    static member inline tryRead x =
        match x with
        | JsonValue.Number n -> Ok (explicit n)
        | JsonValue.Float  n -> Ok (explicit n)
        | js                 -> Decode.Fail.numExpected (FdEncoding js)

    
   

    /// Unwraps the JsonValue inside an IEncoding
    static member Unwrap (x: IEncoding) = x :?> FdEncoding |> fun (FdEncoding s) -> s

    /// Wraps a JsonValue inside an IEncoding
    static member Wrap x = FdEncoding x :> IEncoding

    static member toIRawCodec (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((FdEncoding.Unwrap >> Ok) <-> FdEncoding.Wrap)
    static member ofIRawCodec (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((FdEncoding.Wrap >> Ok) <-> FdEncoding.Unwrap)

    static member jsonObjectOfJson = function
        | JObject x -> Ok (dictAsJsonObject x)
        | a -> Decode.Fail.objExpected (FdEncoding a)

    static member jsonOfJsonObject o = JObject o

    static member createTuple c t = function 
        | JArray a as x -> if length a <> c then Decode.Fail.count c (FdEncoding x) else t a
        | a -> Decode.Fail.arrExpected (FdEncoding a)

    
    //////////////
    // Decoders //
    //////////////

    static member resultD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Result<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Ok", a)] -> a |> decoder1 |> Result.map Ok
            | [KeyValue ("Error", a)] -> a |> decoder2 |> Result.map Error
            | _ -> Decode.Fail.invalidValue (FdEncoding jobj) ""
        | a -> Decode.Fail.objExpected (FdEncoding a)

    static member choiceD (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of2", a)] -> a |> decoder1 |> Result.map Choice1Of2
            | [KeyValue ("Choice2Of2", a)] -> a |> decoder2 |> Result.map Choice2Of2
            | _ -> Decode.Fail.invalidValue (FdEncoding jobj) ""
        | a -> Decode.Fail.objExpected (FdEncoding a)

    static member choice3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
        | JObject o as jobj ->
            match Seq.toList o with
            | [KeyValue ("Choice1Of3", a)] -> a |> decoder1 |> Result.map Choice1Of3
            | [KeyValue ("Choice2Of3", a)] -> a |> decoder2 |> Result.map Choice2Of3
            | [KeyValue ("Choice3Of3", a)] -> a |> decoder3 |> Result.map Choice3Of3
            | _ -> Decode.Fail.invalidValue (FdEncoding jobj) ""
        | a     -> Decode.Fail.objExpected (FdEncoding a)

    static member optionD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
        | JNull _ -> Ok None
        | x       -> Result.map Some (decoder x)

    static member nullableD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
        | JNull _ -> Ok (Nullable ())
        | x       -> Result.map Nullable (decoder x)

    static member arrayD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
        | JArray a -> Seq.traverse decoder a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected (FdEncoding a)
        
    static member multiMapD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<MultiObj<'a>> = function
        | JObject o -> Seq.traverse decoder (IReadOnlyDictionary.values o) |> Result.map (fun values -> Seq.zip (IReadOnlyDictionary.keys o) values |> Seq.toList |> List.map KeyValuePair |> multiMap)
        | a         -> Decode.Fail.objExpected (FdEncoding a)

    static member unitD : JsonValue -> ParseResult<unit> =
        FdEncoding.createTuple 0 (fun _ -> (Ok ()))

    static member tuple1D (decoder1: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Tuple<'a>> =
        FdEncoding.createTuple 1 (fun a -> Result.map Tuple (decoder1 a.[0]))

    static member tuple2D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> =
        FdEncoding.createTuple 2 (fun a -> Result.map2 (fun a b -> (a, b)) (decoder1 a.[0]) (decoder2 a.[1]))

    static member tuple3D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> =
        FdEncoding.createTuple 3 (fun a -> Result.map (fun a b c -> (a, b, c)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2])
    
    static member tuple4D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
        FdEncoding.createTuple 4 (fun a -> Result.map (fun a b c d -> (a, b, c, d)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3])
    
    static member tuple5D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
        FdEncoding.createTuple 5 (fun a -> Result.map (fun a b c d e -> (a, b, c, d, e)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4])
    
    static member tuple6D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
        FdEncoding.createTuple 6 (fun a -> Result.map (fun a b c d e f -> (a, b, c, d, e, f)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5])
    
    static member tuple7D (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
        FdEncoding.createTuple 7 (fun a -> Result.map (fun a b c d e f g -> (a, b, c, d, e, f, g)) (decoder1 a.[0]) <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6])

    static member decimalD x = FdEncoding.tryRead<decimal> x
    static member int16D   x = FdEncoding.tryRead<int16>   x
    static member intD     x = FdEncoding.tryRead<int>     x
    static member int64D   x = FdEncoding.tryRead<int64>   x
    static member uint16D  x = FdEncoding.tryRead<uint16>  x
    static member uint32D  x = FdEncoding.tryRead<uint32>  x
    static member uint64D  x = FdEncoding.tryRead<uint64>  x
    static member byteD    x = FdEncoding.tryRead<byte>    x
    static member sbyteD   x = FdEncoding.tryRead<sbyte>   x
    static member floatD   x = FdEncoding.tryRead<double>  x
    static member float32D x = FdEncoding.tryRead<single>  x

    static member enumD x : Result< 't, _> when 't: enum<_> =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Enum.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (FdEncoding x) s
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member booleanD x =
        match x with
        | JBool b -> Ok b
        | a -> Decode.Fail.boolExpected (FdEncoding a)

    static member stringD x =
        match x with
        | JString b -> Ok b
        | JNull     -> Ok null
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member charD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> Ok s.[0]
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (FdEncoding x) s
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member dateTimeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (FdEncoding x) ""
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (FdEncoding x) ""
        | a -> Decode.Fail.strExpected (FdEncoding a)

    static member timeSpanD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JNumber _ as j -> FdEncoding.int64D j |> Result.map TimeSpan
        | a -> Decode.Fail.numExpected (FdEncoding a)


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

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = FdEncoding.resultD (dec codec1) (dec codec2) <-> FdEncoding.resultE (enc codec1) (enc codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = FdEncoding.choiceD (dec codec1) (dec codec2) <-> FdEncoding.choiceE (enc codec1) (enc codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = FdEncoding.choice3D (dec codec1) (dec codec2) (dec codec3) <-> FdEncoding.choice3E (enc codec1) (enc codec2) (enc codec3)
    static member option (codec: Codec<_,_>) = FdEncoding.optionD (dec codec) <-> FdEncoding.optionE (enc codec)
    static member nullable (codec: Codec<JsonValue, 't>) = FdEncoding.nullableD (dec codec) <-> FdEncoding.nullableE (enc codec) : Codec<JsonValue, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = FdEncoding.arrayD  (dec codec) <-> FdEncoding.arrayE    (enc codec)
    static member multiMap (codec: Codec<_,_>) = FdEncoding.multiMapD (dec codec) <-> FdEncoding.multiMapE (enc codec)

    static member unit () = FdEncoding.unitD <-> FdEncoding.unitE
    static member tuple1 (codec1: Codec<_,_>)                                                                                                                               = FdEncoding.tuple1D (dec codec1)                                                                               <-> FdEncoding.tuple1E (enc codec1)
    static member tuple2 (codec1: Codec<_,_>) (codec2: Codec<_,_>)                                                                                                          = FdEncoding.tuple2D (dec codec1) (dec codec2)                                                                  <-> FdEncoding.tuple2E (enc codec1) (enc codec2)
    static member tuple3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>)                                                                                     = FdEncoding.tuple3D (dec codec1) (dec codec2) (dec codec3)                                                     <-> FdEncoding.tuple3E (enc codec1) (enc codec2) (enc codec3)
    static member tuple4 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>)                                                                = FdEncoding.tuple4D (dec codec1) (dec codec2) (dec codec3) (dec codec4)                                        <-> FdEncoding.tuple4E (enc codec1) (enc codec2) (enc codec3) (enc codec4)
    static member tuple5 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>)                                           = FdEncoding.tuple5D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5)                           <-> FdEncoding.tuple5E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5)
    static member tuple6 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>)                      = FdEncoding.tuple6D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6)              <-> FdEncoding.tuple6E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6)
    static member tuple7 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) (codec4: Codec<_,_>) (codec5: Codec<_,_>) (codec6: Codec<_,_>) (codec7: Codec<_,_>) = FdEncoding.tuple7D (dec codec1) (dec codec2) (dec codec3) (dec codec4) (dec codec5) (dec codec6) (dec codec7) <-> FdEncoding.tuple7E (enc codec1) (enc codec2) (enc codec3) (enc codec4) (enc codec5) (enc codec6) (enc codec7)

    static member boolean  : Codec<JsonValue, bool>      =  FdEncoding.booleanD <-> FdEncoding.booleanE
    static member string         = FdEncoding.stringD         <-> FdEncoding.stringE
    static member dateTime       = FdEncoding.dateTimeD       <-> FdEncoding.dateTimeE
    static member dateTimeOffset = FdEncoding.dateTimeOffsetD <-> FdEncoding.dateTimeOffsetE
    static member timeSpan       = FdEncoding.timeSpanD       <-> FdEncoding.timeSpanE
    static member decimal        = FdEncoding.decimalD        <-> FdEncoding.decimalE
    static member float          = FdEncoding.floatD          <-> FdEncoding.floatE
    static member float32        = FdEncoding.float32D        <-> FdEncoding.float32E
    static member int            = FdEncoding.intD            <-> FdEncoding.intE
    static member uint32         = FdEncoding.uint32D         <-> FdEncoding.uint32E
    static member int64          = FdEncoding.int64D          <-> FdEncoding.int64E
    static member uint64         = FdEncoding.uint64D         <-> FdEncoding.uint64E
    static member int16          = FdEncoding.int16D          <-> FdEncoding.int16E
    static member uint16         = FdEncoding.uint16D         <-> FdEncoding.uint16E
    static member byte           = FdEncoding.byteD           <-> FdEncoding.byteE
    static member sbyte          = FdEncoding.sbyteD          <-> FdEncoding.sbyteE
    static member char           = FdEncoding.charD           <-> FdEncoding.charE
    static member guid           = FdEncoding.guidD           <-> FdEncoding.guidE


    interface IEncoding with
        member _.unit           = FdEncoding.toIRawCodec (FdEncoding.unitD <-> FdEncoding.unitE)
        member _.boolean        = FdEncoding.toIRawCodec FdEncoding.boolean
        member _.string         = FdEncoding.toIRawCodec FdEncoding.string
        member _.dateTime       = FdEncoding.toIRawCodec FdEncoding.dateTime
        member _.dateTimeOffset = FdEncoding.toIRawCodec FdEncoding.dateTimeOffset
        member _.timeSpan       = FdEncoding.toIRawCodec FdEncoding.timeSpan
        member _.decimal        = FdEncoding.toIRawCodec FdEncoding.decimal
        member _.float          = FdEncoding.toIRawCodec FdEncoding.float
        member _.float32        = FdEncoding.toIRawCodec FdEncoding.float32
        member _.int            = FdEncoding.toIRawCodec FdEncoding.int
        member _.uint32         = FdEncoding.toIRawCodec FdEncoding.uint32
        member _.int64          = FdEncoding.toIRawCodec FdEncoding.int64
        member _.uint64         = FdEncoding.toIRawCodec FdEncoding.uint64
        member _.int16          = FdEncoding.toIRawCodec FdEncoding.int16
        member _.uint16         = FdEncoding.toIRawCodec FdEncoding.uint16
        member _.byte           = FdEncoding.toIRawCodec FdEncoding.byte
        member _.sbyte          = FdEncoding.toIRawCodec FdEncoding.sbyte
        member _.char           = FdEncoding.toIRawCodec FdEncoding.char
        member _.guid           = FdEncoding.toIRawCodec FdEncoding.guid

        member _.result c1 c2     = FdEncoding.toIRawCodec (FdEncoding.result   (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2))
        member _.choice c1 c2     = FdEncoding.toIRawCodec (FdEncoding.choice   (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2))
        member _.choice3 c1 c2 c3 = FdEncoding.toIRawCodec (FdEncoding.choice3  (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3))
        member _.option c         = FdEncoding.toIRawCodec (FdEncoding.option   (FdEncoding.ofIRawCodec c))
        member _.nullable c       = FdEncoding.toIRawCodec (FdEncoding.nullable (FdEncoding.ofIRawCodec c))
        member _.array c          = FdEncoding.toIRawCodec (FdEncoding.array    (FdEncoding.ofIRawCodec c))
        member _.multiMap c       = FdEncoding.toIRawCodec (FdEncoding.multiMap (FdEncoding.ofIRawCodec c))

        member _.tuple1 c                    = FdEncoding.toIRawCodec (FdEncoding.tuple1 (FdEncoding.ofIRawCodec c))
        member _.tuple2 c1 c2                = FdEncoding.toIRawCodec (FdEncoding.tuple2 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2))
        member _.tuple3 c1 c2 c3             = FdEncoding.toIRawCodec (FdEncoding.tuple3 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3))
        member _.tuple4 c1 c2 c3 c4          = FdEncoding.toIRawCodec (FdEncoding.tuple4 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3) (FdEncoding.ofIRawCodec c4))
        member _.tuple5 c1 c2 c3 c4 c5       = FdEncoding.toIRawCodec (FdEncoding.tuple5 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3) (FdEncoding.ofIRawCodec c4) (FdEncoding.ofIRawCodec c5))
        member _.tuple6 c1 c2 c3 c4 c5 c6    = FdEncoding.toIRawCodec (FdEncoding.tuple6 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3) (FdEncoding.ofIRawCodec c4) (FdEncoding.ofIRawCodec c5) (FdEncoding.ofIRawCodec c6))
        member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = FdEncoding.toIRawCodec (FdEncoding.tuple7 (FdEncoding.ofIRawCodec c1) (FdEncoding.ofIRawCodec c2) (FdEncoding.ofIRawCodec c3) (FdEncoding.ofIRawCodec c4) (FdEncoding.ofIRawCodec c5) (FdEncoding.ofIRawCodec c6) (FdEncoding.ofIRawCodec c7))

        // Requires F# 5.0
        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> (_: Codec<IEncoding, 'u>) : Codec<IEncoding, 't> = FdEncoding.toIRawCodec (FdEncoding.enumD <-> FdEncoding.enumE)

        member x.getCase =
            match x with
            | FdEncoding (JNull    ) -> "JNull"
            | FdEncoding (JBool   _) -> "JBool" 
            | FdEncoding (JNumber _) -> "JNumber"
            | FdEncoding (JString _) -> "JString"
            | FdEncoding (JArray  _) -> "JArray"
            | FdEncoding (JObject _) -> "JObject"


[<AutoOpen>]
module Main =

    ///////////////////////
    // Main entry points //
    ///////////////////////

    /// Get the json encoding representation of the value, using its default codec.
    let inline toJson (x: 'T) : FdEncoding = toEncoding<FdEncoding, 'T> x

    /// Attempts to decode the value from its json encoding representation, using its default codec.
    let inline ofJson (x: FdEncoding) : Result<'T, DecodeError> = ofEncoding x

    /// Get the json value representation of the value, using its default codec.
    let inline toJsonValue (x: 'T) : JsonValue = toEncoding<FdEncoding, 'T> x |> FdEncoding.Unwrap

    /// Attempts to decode the value from its json value representation, using its default codec.
    let inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (FdEncoding x)

    /// Get the json text representation of the value, using its default codec.
    let inline toJsonText (x: FdEncoding) = x |> toJson |> string

    /// Attempts to decode the value from its json text representation, using its default codec.
    let inline ofJsonText (x: string) = try Ok (FdEncoding.Parse x) with e -> Decode.Fail.parseError e x