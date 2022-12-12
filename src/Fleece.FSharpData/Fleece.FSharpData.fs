namespace Fleece.FSharpData

open System
open System.Collections.Generic
open FSharp.Data

[<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>]
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
        static member create (x: bigint ) : JsonValue = JsonValue.String (string  x)
        static member create (x: Guid   ) : JsonValue = JsonValue.String (string  x)


    type JsonObject = Fleece.PropertyList<JsonValue>
        
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
open Internals


type [<Struct>] Encoding = Encoding of JsonValue with
    override this.ToString () = let (Encoding x) = this in x.ToString ()
    static member Parse (x: string) = Encoding (JsonValue.Parse x)

    static member inline tryRead x =
        match x with
        | JsonValue.Number n -> Ok (explicit n)
        | JsonValue.Float  n -> Ok (explicit n)
        | js                 -> Decode.Fail.numExpected (Encoding js)

    /// Unwraps the JsonValue inside an IEncoding
    static member Unwrap (x: IEncoding) = x :?> Encoding |> fun (Encoding s) -> s

    /// Wraps a JsonValue inside an IEncoding
    static member Wrap x = Encoding x :> IEncoding

    static member toIEncoding (c: Codec<JsonValue, 't>) : Codec<IEncoding, 't> = c |> Codec.compose ((Encoding.Unwrap >> Ok) <-> Encoding.Wrap)
    static member ofIEncoding (c: Codec<IEncoding, 't>) : Codec<JsonValue, 't> = c |> Codec.compose ((Encoding.Wrap >> Ok) <-> Encoding.Unwrap)

    static member jsonObjectOfJson = function
        | JObject x -> Ok (dictAsJsonObject x)
        | a -> Decode.Fail.objExpected (Encoding a)

    static member jsonOfJsonObject o = JObject o

    
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
        | JArray a -> traversei (fun i -> decoder >> Result.bindError (Decode.Fail.inner ($"#{i}"))) a |> Result.map Seq.toArray
        | a        -> Decode.Fail.arrExpected (Encoding a)
        
    static member propListD (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<PropertyList<'a>> = function
        | JObject o -> traversei (fun i -> decoder >> Result.bindError (Decode.Fail.inner i)) (o |> Seq.map (|KeyValue|) |> toArray |> PropertyList)
        | a         -> Decode.Fail.objExpected (Encoding a)

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

    static member bigintD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match tryParse s with (Some (value: bigint)) -> Ok value | _ -> Decode.Fail.invalidValue (Encoding x) s
        | a -> Decode.Fail.strExpected (Encoding a)

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue (Encoding x) s
        | a -> Decode.Fail.strExpected (Encoding a)

    static member dateD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [|"yyyy-MM-dd" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (Encoding x) ""
        | a -> Decode.Fail.strExpected (Encoding a)

    static member timeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| "HH:mm:ss.fff"; "HH:mm:ss" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (Encoding x) ""
        | a -> Decode.Fail.strExpected (Encoding a)

    static member dateTimeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue (Encoding x) ""
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
    static member arrayE    (encoder: _ -> JsonValue) (x: 'a [])        = JArray (Array.map encoder x |> Array.toList)
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
    static member dateE           (x: DateTime      ) = JString (x.ToString "yyyy-MM-dd")
    static member timeE           (x: DateTime      ) = JString (x.ToString "HH:mm:ss.fff")
    static member dateTimeE       (x: DateTime      ) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffZ"))
    static member dateTimeOffsetE (x: DateTimeOffset) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffK"))
    static member timeSpanE       (x: TimeSpan      ) = JsonHelpers.create x.Ticks

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
    static member bigintE         (x: bigint        ) = JsonHelpers.create x
    static member guidE           (x: Guid          ) = JsonHelpers.create x

    
    ////////////
    // Codecs //
    ////////////

    static member result  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.resultD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.resultE (Codec.encode codec1) (Codec.encode codec2)

    static member choice  (codec1: Codec<_,_>) (codec2: Codec<_,_>) = Encoding.choiceD (Codec.decode codec1) (Codec.decode codec2) <-> Encoding.choiceE (Codec.encode codec1) (Codec.encode codec2)
    static member choice3 (codec1: Codec<_,_>) (codec2: Codec<_,_>) (codec3: Codec<_,_>) = Encoding.choice3D (Codec.decode codec1) (Codec.decode codec2) (Codec.decode codec3) <-> Encoding.choice3E (Codec.encode codec1) (Codec.encode codec2) (Codec.encode codec3)
    static member option   (codec: Codec<_,_>) = Encoding.optionD (Codec.decode codec) <-> Encoding.optionE (Codec.encode codec)
    static member nullable (codec: Codec<JsonValue, 't>) = Encoding.nullableD (Codec.decode codec) <-> Encoding.nullableE (Codec.encode codec) : Codec<JsonValue, Nullable<'t>>
    static member array    (codec: Codec<_,_>) = Encoding.arrayD    (Codec.decode codec) <-> Encoding.arrayE    (Codec.encode codec)
    static member multiMap (codec: Codec<_,_>) = Encoding.propListD (Codec.decode codec) <-> Encoding.propListE (Codec.encode codec)

    static member boolean        = Encoding.booleanD        <-> Encoding.booleanE
    static member string         = Encoding.stringD         <-> Encoding.stringE
    static member date           = Encoding.dateD           <-> Encoding.dateE
    static member time           = Encoding.timeD           <-> Encoding.timeE
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
    static member bigint         = Encoding.bigintD         <-> Encoding.bigintE
    static member guid           = Encoding.guidD           <-> Encoding.guidE


    interface IEncoding with
        member _.boolean        = Encoding.toIEncoding Encoding.boolean
        member _.string         = Encoding.toIEncoding Encoding.string
        member _.dateTime t     =
            match t with            
            | Some DateTimeContents.Date -> Encoding.toIEncoding Encoding.date
            | Some DateTimeContents.Time -> Encoding.toIEncoding Encoding.time
            | _                          -> Encoding.toIEncoding Encoding.dateTime
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
        member _.bigint         = Encoding.toIEncoding Encoding.bigint
        member _.guid           = Encoding.toIEncoding Encoding.guid

        member _.result c1 c2     = Encoding.toIEncoding (Encoding.result   (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2))
        member _.choice c1 c2     = Encoding.toIEncoding (Encoding.choice   (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2))
        member _.choice3 c1 c2 c3 = Encoding.toIEncoding (Encoding.choice3  (Encoding.ofIEncoding c1) (Encoding.ofIEncoding c2) (Encoding.ofIEncoding c3))
        member _.option c         = Encoding.toIEncoding (Encoding.option   (Encoding.ofIEncoding c))
        member _.array c          = Encoding.toIEncoding (Encoding.array    (Encoding.ofIEncoding c))
        member _.propertyList c   = Encoding.toIEncoding (Encoding.multiMap (Encoding.ofIEncoding c))

        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> () : Codec<IEncoding, 't> = Encoding.toIEncoding (Encoding.enumD <-> Encoding.enumE)

        member x.getCase =
            match x with
            | Encoding (JNull    ) -> "JNull"
            | Encoding (JBool   _) -> "JBool" 
            | Encoding (JNumber _) -> "JNumber"
            | Encoding (JString _) -> "JString"
            | Encoding (JArray  _) -> "JArray"
            | Encoding (JObject _) -> "JObject"