namespace Fleece.SystemTextJson

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
        | JsonValueKind.Array     -> JArray [ for x in o.EnumerateArray () -> (^Encoding : (static member Wrap: _ -> ^Encoding) x) ]
        | JsonValueKind.Object    -> JObject (PropertyList [|for x in o.EnumerateObject () -> (x.Name, (^Encoding : (static member Wrap: _ -> ^Encoding) x.Value))|] :> IReadOnlyDictionary<_,_>)
        | JsonValueKind.Number    -> JNumber j
        | JsonValueKind.False     -> JBool false
        | JsonValueKind.True      -> JBool true
        | JsonValueKind.String    -> JString (o.GetString ())
        | _                       -> failwithf "Invalid Encoding %A" o

    [<Struct>]
    type JsonElementOrWriter =
    | Element of ElementValue: JsonElement
    | Writer  of WriterValue: (Utf8JsonWriter -> string option -> unit)

open Internals

type JsonObject = PropertyList<Encoding>


/// Wrapper type for JsonElement
and Encoding (j: JsonElementOrWriter) =
    
    let mutable Value = j
    static let mutable dateFormat           = "yyyy-MM-dd"
    static let mutable timeFormat           = "HH:mm:ss.fff"
    static let mutable dateTimeFormat       = "yyyy-MM-ddTHH:mm:ss.fffZ"
    static let mutable dateTimeOffsetFormat = "yyyy-MM-ddTHH:mm:ss.fffK"
    static member DateFormat           with get () = dateFormat           and set (v) = dateFormat <- v
    static member TimeFormat           with get () = timeFormat           and set (v) = timeFormat <- v
    static member DateTimeFormat       with get () = dateTimeFormat       and set (v) = dateTimeFormat <- v
    static member DateTimeOffsetFormat with get () = dateTimeOffsetFormat and set (v) = dateTimeOffsetFormat <- v

    new () = Encoding (Unchecked.defaultof<_>)

    static member Wrap x = Encoding (Element x)

    member _.ToString (options: JsonWriterOptions) =
        use stream = new System.IO.MemoryStream ()
        use writer = new Utf8JsonWriter (stream, options)
        use reader = new System.IO.StreamReader (stream)
        match Value with
        | Writer jobj   -> jobj writer None
        | Element value -> value.WriteTo writer
        writer.Flush ()
        stream.Seek (0L, System.IO.SeekOrigin.Begin) |> ignore
        reader.ReadToEnd ()

    override this.ToString () = this.ToString (new JsonWriterOptions (Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping))

    static member Parse (x: string) = let doc = JsonDocument.Parse x in Encoding (Element doc.RootElement)

    member this.get_InnerValue () =
        match Value with
        | Element value -> value
        | Writer  _     ->
            // run the function, then parseback
            let str = string this
            let doc = JsonDocument.Parse str
            let value = doc.RootElement
            Value <- Element value
            value

    member _.getWriter () =
        match Value with
        | Writer writer -> writer
        | Element value ->
            fun (writer: Utf8JsonWriter) (name: string option) ->
                name |> Option.iter writer.WritePropertyName
                value.WriteTo writer

    static member inline private writers keyValueWriter valueWriter = Encoding (Writer (fun (writer: Utf8JsonWriter) -> function Some name -> keyValueWriter writer name | _ -> valueWriter writer))

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
    static member create (x: int16  ) = Encoding.writers (fun w k -> w.WriteNumber (k,  int32 x)) (fun w -> w.WriteNumberValue ( int32 x))
    static member create (x: uint16 ) = Encoding.writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
    static member create (x: byte   ) = Encoding.writers (fun w k -> w.WriteNumber (k, uint32 x)) (fun w -> w.WriteNumberValue (uint32 x))
    static member create (x: sbyte  ) = Encoding.writers (fun w k -> w.WriteNumber (k,  int32 x)) (fun w -> w.WriteNumberValue ( int32 x))
    static member create (x: char   ) = Encoding.writers (fun w k -> w.WriteString (k, string x)) (fun w -> w.WriteStringValue (string x))
    static member create (x: bigint ) =
        let f (w: Utf8JsonWriter) (k: option<string>) =
            k |> Option.iter w.WritePropertyName
            let doc = JsonDocument.Parse (string x)
            doc.WriteTo w
        Encoding (Writer f)

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
    static member ($) (_:Encoding, _: bigint ) = fun (x: Encoding) -> System.Numerics.BigInteger.Parse (x.get_InnerValue().GetRawText (), NumberFormatInfo.InvariantInfo)

    static member inline tryGet (x: Encoding) : 't = (Unchecked.defaultof<Encoding> $ Unchecked.defaultof<'t>) x

    static member inline tryRead (x: Encoding) =
        match x with
        | JNumber j ->
            try 
                Ok (Encoding.tryGet j)
            with e -> Decode.Fail.invalidValue x (string e)
        | js -> Decode.Fail.numExpected js

    /// Downcasts IEncoding to a SystemTextJson.Encoding
    static member Unwrap (x: IEncoding) = (x :?> Encoding).get_InnerValue ()

    static member toIEncoding (c: Codec<Encoding, 't>) : Codec<IEncoding, 't> = c |> Codec.upCast
    static member ofIEncoding (c: Codec<IEncoding, 't>) : Codec<Encoding, 't> = c |> Codec.downCast


    static member jsonObjectOfJson = function
        | JObject x -> Ok (Encoding.dictAsJsonObject x)
        | a -> Decode.Fail.objExpected a

    static member jsonOfJsonObject (o: JsonObject) = Encoding.JObject o

    
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

    static member bigintD x = Encoding.tryRead<bigint> x

    static member guidD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    -> match Guid.TryParse s with (true, value) -> Ok value | _ -> Decode.Fail.invalidValue x s
        | a -> Decode.Fail.strExpected a

    static member dateD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [|dateFormat;  "yyyy-MM-dd" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue x ""
        | a -> Decode.Fail.strExpected a

    static member timeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| timeFormat; "HH:mm:ss" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue x ""
        | a -> Decode.Fail.strExpected a

    static member dateTimeD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTime.TryParseExact (s, [| dateTimeFormat; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
            | true, t -> Ok t
            | _       -> Decode.Fail.invalidValue x ""
        | a -> Decode.Fail.strExpected a

    static member dateTimeOffsetD x =
        match x with
        | JString null -> Decode.Fail.nullString
        | JString s    ->
            match DateTimeOffset.TryParseExact (s, [| dateTimeOffsetFormat; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
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
    static member dateE           (x: DateTime      ) = Encoding.JString (x.ToString dateFormat)
    static member timeE           (x: DateTime      ) = Encoding.JString (x.ToString timeFormat)
    static member dateTimeE       (x: DateTime      ) = Encoding.JString (x.ToString dateTimeFormat)
    static member dateTimeOffsetE (x: DateTimeOffset) = Encoding.JString (x.ToString dateTimeOffsetFormat)
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
    static member bigintE         (x: bigint        ) = Encoding.create x
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
        member _.propertyList c   = Encoding.toIEncoding (Encoding.propList (Encoding.ofIEncoding c))
        
        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> () : Codec<IEncoding, 't> = Encoding.toIEncoding (Encoding.enumD <-> Encoding.enumE)
        
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
    let inline JArray  (x: IReadOnlyList<JsonElement>) = x |> Seq.map Encoding.Wrap |> Seq.toList|> Encoding.JArray |> Encoding.Unwrap
    let inline JObject (x: IReadOnlyDictionary<string, JsonElement>) = x |> IReadOnlyDictionary.map Encoding.Wrap |> Encoding.JObject |> Encoding.Unwrap
    let        JBool x   = Encoding.JBool x   |> Encoding.Unwrap
    let        JNull     = Encoding.JNull     |> Encoding.Unwrap
    let        JString x = Encoding.JString x |> Encoding.Unwrap
    let        JNumber x = Encoding.JNumber x |> Encoding.Unwrap
    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonElement) =
        match o.ValueKind with
        | JsonValueKind.Null
        | JsonValueKind.Undefined -> JNull
        | JsonValueKind.Array     -> JArray [ for x in o.EnumerateArray () ->  x ]
        | JsonValueKind.Object    -> JObject (PropertyList [|for x in o.EnumerateObject () -> (x.Name,  x.Value)|] :> IReadOnlyDictionary<_,_>)
        | JsonValueKind.Number    -> JNumber o
        | JsonValueKind.False     -> JBool false
        | JsonValueKind.True      -> JBool true
        | JsonValueKind.String    -> JString (o.GetString ())
        | _                       -> failwithf "Invalid Encoding %A" o