namespace Fleece

open System
open System.Globalization
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data

type Id1<'t> (v: 't) =
    let value = v
    member __.getValue = value

type Id2<'t> (v: 't) =
    let value = v
    member __.getValue = value

type Default7 = class end
type Default6 = class inherit Default7 end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

#if FSHARPDATA

module FSharpData =
    
    open FSharp.Data
        
    type JsonObject = (string * JsonValue) []
    
    type private JsonHelpers () =
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


    type private ReadOnlyJsonPropertiesDictionary (properties: (string * JsonValue) []) =
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


    let jsonObjectGetValues (x: JsonObject) = ReadOnlyJsonPropertiesDictionary x :> IReadOnlyDictionary<string, JsonValue>


    // FSharp.Data.JsonValue AST adapter

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonValue) =
        match o with
        | JsonValue.Null          -> JNull
        | JsonValue.Array els     -> JArray (IList.toIReadOnlyList els)
        | JsonValue.Record props  -> JObject (jsonObjectGetValues props)
        | JsonValue.Number _ as x -> JNumber x
        | JsonValue.Float _ as x  -> JNumber x
        | JsonValue.Boolean x     -> JBool x
        | JsonValue.String x      -> JString x
    
    let dictAsProps (x: IReadOnlyDictionary<string, JsonValue>) =
        match x with
        | :? ReadOnlyJsonPropertiesDictionary as x' -> x'.Properties
        | _ -> x |> Seq.map (|KeyValue|) |> Array.ofSeq

    let inline JArray (x: JsonValue IReadOnlyList) = JsonValue.Array (x |> Array.ofSeq)
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonValue.Record (dictAsProps x)
    let inline JBool (x: bool) = JsonValue.Boolean x
    let inline JNumber (x: decimal) = JsonValue.Number x
    let JNull : JsonValue = JsonValue.Null
    let inline JString (x: string) = if isNull x then JsonValue.Null else JsonValue.String x
    
#endif

#if NEWTONSOFT

module Newtonsoft =
    
    open Newtonsoft.Json.Linq
    type JsonValue = JToken
    type JObject with
        member x.AsReadOnlyDictionary () = (x.Properties () |> Seq.map (fun p -> (p.Name, p.Value)) |> dict) |> Dict.toIReadOnlyDictionary
        static member GetValues (x: JObject) = x.AsReadOnlyDictionary ()

    let jsonObjectGetValues (x : JObject) = JObject.GetValues x

    type JsonObject = JObject
    
    type private JsonHelpers () =
        static member create (x: decimal) = JValue          x  :> JToken
        static member create (x: Double ) = JValue          x  :> JToken
        static member create (x: Single ) = JValue          x  :> JToken
        static member create (x: int    ) = JValue          x  :> JToken
        static member create (x: uint32 ) = JValue          x  :> JToken
        static member create (x: int64  ) = JValue          x  :> JToken
        static member create (x: uint64 ) = JValue          x  :> JToken
        static member create (x: int16  ) = JValue          x  :> JToken
        static member create (x: uint16 ) = JValue          x  :> JToken
        static member create (x: byte   ) = JValue          x  :> JToken
        static member create (x: sbyte  ) = JValue          x  :> JToken
        static member create (x: char   ) = JValue (string  x) :> JToken
        static member create (x: Guid   ) = JValue (string  x) :> JToken


    // FSharp.Newtonsoft.Json AST adapter

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JToken) =
        match o.Type with
        | JTokenType.Null    -> JNull
        | JTokenType.Array   -> JArray ((o :?> JArray) |> IList.toIReadOnlyList)
        | JTokenType.Object  -> JObject (jsonObjectGetValues (o :?> JObject))
        | JTokenType.Integer -> JNumber  o
        | JTokenType.Float   -> JNumber  o
        | JTokenType.Boolean -> JBool   (o.ToObject () : bool)
        | JTokenType.String  -> JString (o.ToObject () : string)
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
    
#endif

#if SYSTEMJSON

module SystemJson =
    
    open System.Json

    type JsonObject with
        member x.AsReadOnlyDictionary () = (x :> IDictionary<string, JsonValue>) |> Dict.toIReadOnlyDictionary
        static member GetValues (x: JsonObject) = x.AsReadOnlyDictionary ()

    let jsonObjectGetValues (x: JsonObject) = JsonObject.GetValues x

    type private JsonHelpers () =
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

    #endif

    // Deserializing:

    type JType =
        | Object = 1
        | Array  = 2
        | Number = 3
        | String = 4
        | Bool   = 5
        | Null   = 6

    let getJType (o: JsonValue) =
        match o with
        | JNull     -> JType.Null
        | JArray _  -> JType.Array
        | JObject _ -> JType.Object
        | JNumber _ -> JType.Number
        | JBool _   -> JType.Bool
        | JString _ -> JType.String

    type DecodeError =
        | JsonTypeMismatch of System.Type * JsonValue * JType * JType
        | NullString of System.Type
        | IndexOutOfRange of int * JsonValue
        | InvalidValue of System.Type * JsonValue * string
        | PropertyNotFound of string * IReadOnlyDictionary<string, JsonValue>
        | ParseError of System.Type * exn * string
        | Uncategorized of string
        | Multiple of DecodeError list

    with
        static member (+) (x, y) =
            match x, y with
            | Multiple x, Multiple y -> Multiple (x @ y)
            | _                      -> Multiple [x; y]
        override x.ToString () =
            match x with
            | JsonTypeMismatch (t, v: JsonValue, expected, actual) -> sprintf "%s expected but got %s while decoding %s as %s" (string expected) (string actual) (string v) (string t)
            | NullString t -> sprintf "Expected %s, got null" (string t)
            | IndexOutOfRange (e, a) -> sprintf "Expected array with %s items, was: %s" (string e) (string a)
            | InvalidValue (t, v, s) -> sprintf "Value %s is invalid for %s%s" (string v) (string t) (if String.IsNullOrEmpty s then "" else " " + s)
            | PropertyNotFound (p, o) -> sprintf "Property: '%s' not found in object '%s'" p (string o)
            | ParseError (t, s, v) -> sprintf "Error decoding %s from  %s: %s" (string v) (string t) (string s)
            | Uncategorized str -> str
            | Multiple lst -> List.map string lst |> String.concat "\r\n"

    type 'a ParseResult = Result<'a, DecodeError>

    module Decode =
        let inline Success x = Ok x
        let (|Success|Failure|) = function
            | Ok    x -> Success x
            | Error x -> Failure x

        module Fail =
            let inline objExpected  v : Result<'t, _> = let a = getJType v in Error (JsonTypeMismatch (typeof<'t>, v, JType.Object, a))
            let inline arrExpected  v : Result<'t, _> = let a = getJType v in Error (JsonTypeMismatch (typeof<'t>, v, JType.Array , a))
            let inline numExpected  v : Result<'t, _> = let a = getJType v in Error (JsonTypeMismatch (typeof<'t>, v, JType.Number, a))
            let inline strExpected  v : Result<'t, _> = let a = getJType v in Error (JsonTypeMismatch (typeof<'t>, v, JType.String, a))
            let inline boolExpected v : Result<'t, _> = let a = getJType v in Error (JsonTypeMismatch (typeof<'t>, v, JType.Bool  , a))
            let [<GeneralizableValue>]nullString<'t> : Result<'t, _> = Error (NullString typeof<'t>)
            let inline count e a = Error (IndexOutOfRange (e, a))
            let invalidValue v o : Result<'t, _> = Error (InvalidValue (typeof<'t>, v, o))
            let propertyNotFound p o = Error (PropertyNotFound (p, o))
            let parseError s v : Result<'t, _> = Error (ParseError (typeof<'t>, s, v))
    
    module Helpers =
        // results:
        let inline Success x = Ok x
        let listAsReadOnly (l: _ list) =
            { new IReadOnlyList<_> with
                member __.Count = l.Length
                member __.Item with get index = l.[index]
                member __.GetEnumerator () = (l :> _ seq).GetEnumerator ()
                member __.GetEnumerator () = (l :> System.Collections.IEnumerable).GetEnumerator () }

        let rdict x = x |> dict |> Dict.toIReadOnlyDictionary

        let keys   (x: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) x
        let values (x: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) x
#if !NETCOREAPP        
        type ArraySegment<'a> with
            member x.ToArray() =
                let array = Array.zeroCreate<'a> x.Count
                Array.Copy(x.Array, x.Offset, array, 0, x.Count)
                array
#endif


        #if FSHARPDATA

        let inline tryRead x =
            match x with
            | JsonValue.Number n -> Success (explicit n)
            | JsonValue.Float  n -> Success (explicit n)
            | js                 -> Decode.Fail.numExpected js

        type JsonHelpers with
            static member jsonObjectOfJson = function
                | JObject x -> Success (dictAsProps x)
                | a -> Decode.Fail.objExpected a

        #endif

        #if NEWTONSOFT

        let inline tryRead<'a> x =
            match x with
            | JNumber j -> 
                try
                  Success (j.ToObject<'a> ())
                with
                | e -> Decode.Fail.invalidValue j (string e)
            | js -> Decode.Fail.numExpected js

        type JsonHelpers with
            static member jsonObjectOfJson =
                fun (o: JToken) ->
                    match o.Type with
                    | JTokenType.Object -> Success (o :?> JObject)
                    | _ -> Decode.Fail.objExpected o
 

        #endif

        #if SYSTEMJSON

        let inline tryRead x =
            match x with
            | JNumber j ->
                try
                    Success (implicit j)
                with e -> Decode.Fail.invalidValue j (string e)
            | js -> Decode.Fail.numExpected js

        type JsonHelpers with
            static member inline jsonObjectOfJson =
                fun (o: JsonValue) ->
                    match box o with
                    | :? JsonObject as x -> Success x
                    | _ -> Decode.Fail.objExpected o

        #endif

    open Helpers

    /// Encodes a value of a generic type 't into a value of raw type 'S.
    type Encoder<'S, 't> = 't -> 'S

    /// Decodes a value of raw type 'S into a value of generic type 't, possibly returning an error.
    type Decoder<'S, 't> = 'S -> ParseResult<'t>

    /// A decoder from raw type 'S1 and encoder to raw type 'S2 for string types 't1 and 't2.
    type Codec<'S1, 'S2, 't1, 't2> = Decoder<'S1, 't1> * Encoder<'S2, 't2>

    /// A decoder from raw type 'S1 and encoder to raw type 'S2 for type 't.
    type Codec<'S1, 'S2, 't> = Codec<'S1, 'S2, 't, 't>

    /// A codec for raw type 'S decoding to strong type 't1 and encoding to strong type 't2.
    type SplitCodec<'S, 't1, 't2> = Codec<'S, 'S, 't1, 't2>

    /// A codec for raw type 'S to strong type 't.
    type Codec<'S, 't> = Codec<'S, 'S, 't>

    type ConcreteCodec<'S1, 'S2, 't1, 't2> = { Decoder : ReaderT<'S1, ParseResult<'t1>>; Encoder : 't2 -> Const<'S2, unit> } with
        static member inline Return f = { Decoder = result f; Encoder = konst <| result () }
        static member inline (<*>) (remainderFields: ConcreteCodec<'S, 'S, 'f ->'r, 'T>, currentField: ConcreteCodec<'S, 'S, 'f, 'T>) =
            {
                Decoder = (remainderFields.Decoder : ReaderT<'S, ParseResult<'f -> 'r>>) <*> currentField.Decoder
                Encoder = fun w -> (remainderFields.Encoder w *> currentField.Encoder w)
            }
        static member inline (<!>) (f, field: ConcreteCodec<'S, 'S, 'f, 'T>) = f <!> field
        static member inline (<|>) (source: ConcreteCodec<'S, 'S, 'f, 'T>, alternative: ConcreteCodec<'S, 'S, 'f, 'T>) =
            {
                Decoder = (source.Decoder : ReaderT<'S, ParseResult<'f>>) <|> alternative.Decoder
                Encoder = fun w -> (source.Encoder w ++ alternative.Encoder w)
            }


    module Codec =

        /// Turns a Codec into another Codec, by mapping it over an isomorphism.
        let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) (r, w) = (contramap f r, map g w)

        let inline compose codec1 codec2 = 
            let (dec1, enc1) = codec1
            let (dec2, enc2) = codec2
            (dec1 >> (=<<) dec2, enc1 << enc2)

        let decode (d: Decoder<'i, 'a>, _) (i: 'i) : ParseResult<'a> = d i
        let encode (_, e: Encoder<'o, 'a>) (a: 'a) : 'o = e a

        let inline toMonoid x = x |> toList
        let inline ofMonoid x = x |> (List.map (|KeyValue|) >> rdict)

        let inline ofConcrete {Decoder = ReaderT d; Encoder = e} = contramap toMonoid d, map ofMonoid (e >> Const.run)
        let inline toConcrete (d: _ -> _, e: _ -> _) = { Decoder = ReaderT (contramap ofMonoid d); Encoder = Const << map toMonoid e }

    let jsonObjToValueCodec = ((function JObject (o: IReadOnlyDictionary<_,_>) -> Ok o | a  -> Decode.Fail.objExpected a) , JObject)
    let jsonValueToTextCodec = (fun x -> try Ok (JsonValue.Parse x) with e -> Decode.Fail.parseError e x), (fun (x: JsonValue) -> string x)

    /// Creates a new Json object for serialization
    let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> rdict)


    [<RequireQualifiedAccess>]
    module JsonDecode =
        let choice (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<Choice<'a, 'b>> = function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> decoder1 |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> decoder2 |> map Choice2Of2
                | _ -> Decode.Fail.invalidValue jobj ""
            | a -> Decode.Fail.objExpected a

        let choice3 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> decoder1 |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> decoder2 |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> decoder3 |> map Choice3Of3
                | _ -> Decode.Fail.invalidValue jobj ""
            | a -> Decode.Fail.objExpected a

        let option (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a option> = function
            | JNull _ -> Success None
            | x       -> map Some (decoder x)

        let nullable (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Nullable<'a>> = function
            | JNull _ -> Success (Nullable ())
            | x       -> map Nullable (decoder x)

        let array (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a array> = function
            | JArray a -> traverse decoder a |> map Seq.toArray
            | a        -> Decode.Fail.arrExpected a
            
        let arraySegment (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a ArraySegment> = function
            | JArray a -> traverse decoder a |> map (Seq.toArray >> ArraySegment<_>)
            | a        -> Decode.Fail.arrExpected a

        let list (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a list> = function
            | JArray a -> traverse decoder a |> map Seq.toList
            | a        -> Decode.Fail.arrExpected a

        let set (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a Set> = function
            | JArray a -> traverse decoder a |> map set
            | a        -> Decode.Fail.arrExpected a

        let resizeArray (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<'a ResizeArray> = function
            | JArray a -> traverse decoder a |> map (fun x -> ResizeArray<_> (x: 'a seq))
            | a        -> Decode.Fail.arrExpected a

        let dictionary (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Dictionary<string, 'a>> = function
            | JObject o -> traverse decoder (values o) |> map (fun values -> Seq.zip (keys o) values |> ofSeq)
            | a -> Decode.Fail.objExpected a

        let map (decoder: JsonValue -> ParseResult<'a>) : JsonValue -> ParseResult<Map<string, 'a>> = function
            | JObject o -> traverse decoder (values o) |> map (fun values -> Seq.zip (keys o) values |> Map.ofSeq)
            | a -> Decode.Fail.objExpected a

        let unit : JsonValue -> ParseResult<unit> = function
            | JArray a as x ->
                if a.Count <> 0 then Decode.Fail.count 0 x
                else Success ()
            | a -> Decode.Fail.arrExpected a

        let tuple2 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) : JsonValue -> ParseResult<'a * 'b> = function
            | JArray a as x ->
                if a.Count <> 2 then Decode.Fail.count 2 x
                else tuple2 <!> decoder1 a.[0] <*> decoder2 a.[1]
            | a -> Decode.Fail.arrExpected a

        let tuple3 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) : JsonValue -> ParseResult<'a * 'b * 'c> = function
            | JArray a as x ->
                if a.Count <> 3 then Decode.Fail.count 3 x
                else tuple3 <!> decoder1 a.[0] <*> decoder2 a.[1] <*> decoder3 a.[2]
            | a -> Decode.Fail.arrExpected a

        let tuple4 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> = function
            | JArray a as x ->
                if a.Count <> 4 then Decode.Fail.count 4 x
                else tuple4 <!> decoder1 a.[0] <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3]
            | a -> Decode.Fail.arrExpected a

        let tuple5 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> = function
            | JArray a as x ->
                if a.Count <> 5 then Decode.Fail.count 5 x
                else tuple5 <!> decoder1 a.[0] <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4]
            | a -> Decode.Fail.arrExpected a

        let tuple6 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> = function
            | JArray a as x ->
                if a.Count <> 6 then Decode.Fail.count 6 x
                else tuple6 <!> decoder1 a.[0] <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5]
            | a -> Decode.Fail.arrExpected a

        let tuple7 (decoder1: JsonValue -> ParseResult<'a>) (decoder2: JsonValue -> ParseResult<'b>) (decoder3: JsonValue -> ParseResult<'c>) (decoder4: JsonValue -> ParseResult<'d>) (decoder5: JsonValue -> ParseResult<'e>) (decoder6: JsonValue -> ParseResult<'f>) (decoder7: JsonValue -> ParseResult<'g>) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> = function
            | JArray a as x ->
                if a.Count <> 7 then Decode.Fail.count 3 x
                else tuple7 <!> decoder1 a.[0] <*> decoder2 a.[1] <*> decoder3 a.[2] <*> decoder4 a.[3] <*> decoder5 a.[4] <*> decoder6 a.[5] <*> decoder7 a.[6]
            | a -> Decode.Fail.arrExpected a
        
        let decimal x = tryRead<decimal> x
        let int16   x = tryRead<int16>   x
        let int     x = tryRead<int>     x
        let int64   x = tryRead<int64>   x
        let uint16  x = tryRead<uint16>  x
        let uint32  x = tryRead<uint32>  x
        let uint64  x = tryRead<uint64>  x
        let byte    x = tryRead<byte>    x
        let sbyte   x = tryRead<sbyte>   x
        let float   x = tryRead<double>  x
        let float32 x = tryRead<single>  x

        let boolean x =
            match x with
            | JBool b -> Success b
            | a -> Decode.Fail.boolExpected a

        let string x =
            match x with
            | JString b -> Success b
            | JNull -> Success null
            | a -> Decode.Fail.strExpected a

        let char x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    -> Success s.[0]
            | a -> Decode.Fail.strExpected a

        let guid x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    -> tryParse<Guid> s |> Operators.option Success (Decode.Fail.invalidValue x "")
            | a -> Decode.Fail.strExpected a

        let dateTime x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    ->
                match DateTime.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
                | true, t -> Success t
                | _       -> Decode.Fail.invalidValue x ""
            | a -> Decode.Fail.strExpected a

        let dateTimeOffset x =
            match x with
            | JString null -> Decode.Fail.nullString
            | JString s    ->
                match DateTimeOffset.TryParseExact (s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
                | true, t -> Success t
                | _       -> Decode.Fail.invalidValue x ""
            | a -> Decode.Fail.strExpected a

    [<RequireQualifiedAccess>]
    module JsonEncode =
        let choice (encoder1: _ -> JsonValue) (encoder2: _ -> JsonValue) = function
            | Choice1Of2 a -> jobj [ "Choice1Of2", encoder1 a ]
            | Choice2Of2 a -> jobj [ "Choice2Of2", encoder2 a ]

        let choice3 (encoder1: _ -> JsonValue) (encoder2: _ -> JsonValue) (encoder3: _ -> JsonValue) = function
            | Choice1Of3 a -> jobj [ "Choice1Of3", encoder1 a ]
            | Choice2Of3 a -> jobj [ "Choice2Of3", encoder2 a ]
            | Choice3Of3 a -> jobj [ "Choice3Of3", encoder3 a ]

        let option (encoder: _ -> JsonValue) = function
            | None   -> JNull
            | Some a -> encoder a

        let nullable    (encoder: _ -> JsonValue) (x: Nullable<'a>) = if x.HasValue then encoder x.Value else JNull
        let array       (encoder: _ -> JsonValue) (x: 'a [])           = JArray ((Array.map encoder x) |> IList.toIReadOnlyList)
        let arraySegment(encoder: _ -> JsonValue) (x: 'a ArraySegment) = JArray ((Array.map encoder (x.ToArray())) |> IList.toIReadOnlyList)
        let list        (encoder: _ -> JsonValue) (x: list<'a>)        = JArray (listAsReadOnly (List.map encoder x))
        let set         (encoder: _ -> JsonValue) (x: Set<'a>)         = JArray (Seq.toIReadOnlyList (Seq.map encoder x))
        let resizeArray (encoder: _ -> JsonValue) (x: ResizeArray<'a>) = JArray (Seq.toIReadOnlyList (Seq.map encoder x))
        let map         (encoder: _ -> JsonValue) (x: Map<string, 'a>) = x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k, v)) -> k, encoder v) |> rdict |> JObject
        let dictionary  (encoder: _ -> JsonValue) (x: Dictionary<string, 'a>) = x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k, v)) -> k, encoder v) |> rdict |> JObject
        
        let unit () = JArray ([||] |> IList.toIReadOnlyList)
        let tuple2 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (a, b) = JArray ([|encoder1 a; encoder2 b|] |> IList.toIReadOnlyList)
        let tuple3 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (a, b, c) = JArray ([|encoder1 a; encoder2 b; encoder3 c|] |> IList.toIReadOnlyList)
        let tuple4 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (a, b, c, d) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d|] |> IList.toIReadOnlyList)
        let tuple5 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (a, b, c, d, e) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e|] |> IList.toIReadOnlyList)
        let tuple6 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (a, b, c, d, e, f) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f|] |> IList.toIReadOnlyList)
        let tuple7 (encoder1: 'a -> JsonValue) (encoder2: 'b -> JsonValue) (encoder3: 'c -> JsonValue) (encoder4: 'd -> JsonValue) (encoder5: 'e -> JsonValue) (encoder6: 'f -> JsonValue) (encoder7: 'g -> JsonValue) (a, b, c, d, e, f, g) = JArray ([|encoder1 a; encoder2 b; encoder3 c; encoder4 d; encoder5 e; encoder6 f; encoder7 g|] |> IList.toIReadOnlyList)

        let boolean        (x: bool          ) = JBool x
        let string         (x: string        ) = JString x
        let dateTime       (x: DateTime      ) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffZ")) // JsonPrimitive is incorrect for DateTime
        let dateTimeOffset (x: DateTimeOffset) = JString (x.ToString ("yyyy-MM-ddTHH:mm:ss.fffK")) // JsonPrimitive is incorrect for DateTimeOffset
        let decimal        (x: decimal       ) = JsonHelpers.create x
        let float          (x: Double        ) = JsonHelpers.create x
        let float32        (x: Single        ) = JsonHelpers.create x
        let int            (x: int           ) = JsonHelpers.create x
        let uint32         (x: uint32        ) = JsonHelpers.create x
        let int64          (x: int64         ) = JsonHelpers.create x
        let uint64         (x: uint64        ) = JsonHelpers.create x
        let int16          (x: int16         ) = JsonHelpers.create x
        let uint16         (x: uint16        ) = JsonHelpers.create x
        let byte           (x: byte          ) = JsonHelpers.create x
        let sbyte          (x: sbyte         ) = JsonHelpers.create x
        let char           (x: char          ) = JsonHelpers.create x
        let guid           (x: Guid          ) = JsonHelpers.create x

    [<RequireQualifiedAccess>]
    module JsonCodec =
     
        let choice  codec1 codec2 = JsonDecode.choice (fst codec1) (fst codec2), JsonEncode.choice (snd codec1) (snd codec2)
        let choice3 codec1 codec2 codec3 = JsonDecode.choice3 (fst codec1) (fst codec2) (fst codec3), JsonEncode.choice3 (snd codec1) (snd codec2) (snd codec3)
        let option codec = JsonDecode.option (fst codec), JsonEncode.option (snd codec)
        let nullable codec = JsonDecode.nullable (fst codec), JsonEncode.nullable (snd codec)
        let array codec = JsonDecode.array (fst codec), JsonEncode.array (snd codec)
        let arraySegment codec = JsonDecode.array (fst codec), JsonEncode.arraySegment (snd codec)
        let list  codec = JsonDecode.list  (fst codec), JsonEncode.list  (snd codec)
        let set         codec = JsonDecode.set         (fst codec), JsonEncode.set         (snd codec)
        let resizeArray codec = JsonDecode.resizeArray (fst codec), JsonEncode.resizeArray (snd codec)
        let map         codec = JsonDecode.map         (fst codec), JsonEncode.map         (snd codec)
        let dictionary  codec = JsonDecode.dictionary  (fst codec), JsonEncode.dictionary  (snd codec)

        let unit  ()                                                = JsonDecode.unit                                                                                             , JsonEncode.unit ()
        let tuple2 codec1 codec2                                    = JsonDecode.tuple2 (fst codec1) (fst codec2)                                                                 , JsonEncode.tuple2 (snd codec1) (snd codec2)
        let tuple3 codec1 codec2 codec3                             = JsonDecode.tuple3 (fst codec1) (fst codec2) (fst codec3)                                                    , JsonEncode.tuple3 (snd codec1) (snd codec2) (snd codec3)
        let tuple4 codec1 codec2 codec3 codec4                      = JsonDecode.tuple4 (fst codec1) (fst codec2) (fst codec3) (fst codec4)                                       , JsonEncode.tuple4 (snd codec1) (snd codec2) (snd codec3) (snd codec4)
        let tuple5 codec1 codec2 codec3 codec4 codec5               = JsonDecode.tuple5 (fst codec1) (fst codec2) (fst codec3) (fst codec4) (fst codec5)                          , JsonEncode.tuple5 (snd codec1) (snd codec2) (snd codec3) (snd codec4) (snd codec5)
        let tuple6 codec1 codec2 codec3 codec4 codec5 codec6        = JsonDecode.tuple6 (fst codec1) (fst codec2) (fst codec3) (fst codec4) (fst codec5) (fst codec6)             , JsonEncode.tuple6 (snd codec1) (snd codec2) (snd codec3) (snd codec4) (snd codec5) (snd codec6)
        let tuple7 codec1 codec2 codec3 codec4 codec5 codec6 codec7 = JsonDecode.tuple7 (fst codec1) (fst codec2) (fst codec3) (fst codec4) (fst codec5) (fst codec6) (fst codec7), JsonEncode.tuple7 (snd codec1) (snd codec2) (snd codec3) (snd codec4) (snd codec5) (snd codec6) (snd codec7)

        let boolean        = JsonDecode.boolean       , JsonEncode.boolean
        let string         = JsonDecode.string        , JsonEncode.string
        let dateTime       = JsonDecode.dateTime      , JsonEncode.dateTime
        let dateTimeOffset = JsonDecode.dateTimeOffset, JsonEncode.dateTimeOffset
        let decimal        = JsonDecode.decimal       , JsonEncode.decimal
        let float          = JsonDecode.float         , JsonEncode.float
        let float32        = JsonDecode.float32       , JsonEncode.float32
        let int            = JsonDecode.int           , JsonEncode.int
        let uint32         = JsonDecode.uint32        , JsonEncode.uint32
        let int64          = JsonDecode.int64         , JsonEncode.int64
        let uint64         = JsonDecode.uint64        , JsonEncode.uint64
        let int16          = JsonDecode.int16         , JsonEncode.int16
        let uint16         = JsonDecode.uint16        , JsonEncode.uint16
        let byte           = JsonDecode.byte          , JsonEncode.byte
        let sbyte          = JsonDecode.sbyte         , JsonEncode.sbyte
        let char           = JsonDecode.char          , JsonEncode.char
        let guid           = JsonDecode.guid          , JsonEncode.guid


    // Deserializing:

    type OfJson =
        inherit Default1
        
        static member OfJson (_: decimal, _: OfJson) = JsonDecode.decimal
        static member OfJson (_: int16  , _: OfJson) = JsonDecode.int16
        static member OfJson (_: int    , _: OfJson) = JsonDecode.int
        static member OfJson (_: int64  , _: OfJson) = JsonDecode.int64
        static member OfJson (_: uint16 , _: OfJson) = JsonDecode.uint16
        static member OfJson (_: uint32 , _: OfJson) = JsonDecode.uint32
        static member OfJson (_: uint64 , _: OfJson) = JsonDecode.uint64
        static member OfJson (_: byte   , _: OfJson) = JsonDecode.byte
        static member OfJson (_: sbyte  , _: OfJson) = JsonDecode.sbyte
        static member OfJson (_: double , _: OfJson) = JsonDecode.float
        static member OfJson (_: single , _: OfJson) = JsonDecode.float32

        static member OfJson (_: bool          , _: OfJson) = JsonDecode.boolean
        static member OfJson (_: string        , _: OfJson) = JsonDecode.string
        static member OfJson (_: char          , _: OfJson) = JsonDecode.char
        static member OfJson (_: Guid          , _: OfJson) = JsonDecode.guid
        static member OfJson (_: DateTime      , _: OfJson) = JsonDecode.dateTime
        static member OfJson (_: DateTimeOffset, _: OfJson) = JsonDecode.dateTimeOffset
        static member OfJson (_: unit          , _: OfJson) = JsonDecode.unit

    type OfJson with
        static member inline Invoke (x: JsonValue) : 't ParseResult =
            let inline iOfJson (a: ^a, b: ^b) = ((^a or ^b) : (static member OfJson : ^b * _ -> (JsonValue -> ^b ParseResult)) b, a)
            iOfJson (Unchecked.defaultof<OfJson>, Unchecked.defaultof<'t>) x

    type OfJson with static member inline OfJson (_: Choice<'a, 'b>    , _: OfJson) : JsonValue -> ParseResult<Choice<'a, 'b>>     = JsonDecode.choice  OfJson.Invoke OfJson.Invoke
    type OfJson with static member inline OfJson (_: Choice<'a, 'b, 'c>, _: OfJson) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> = JsonDecode.choice3 OfJson.Invoke OfJson.Invoke OfJson.Invoke

    type OfJson with static member inline OfJson (_: 'a option  , _: OfJson) : JsonValue -> ParseResult<'a option>   = JsonDecode.option   OfJson.Invoke
    type OfJson with static member inline OfJson (_: 'a Nullable, _: OfJson) : JsonValue -> ParseResult<'a Nullable> = JsonDecode.nullable OfJson.Invoke

    type OfJson with static member inline OfJson (_: 'a array, _: OfJson) : JsonValue -> ParseResult<'a array> = JsonDecode.array OfJson.Invoke
    type OfJson with static member inline OfJson (_: 'a ArraySegment, _: OfJson) : JsonValue -> ParseResult<'a ArraySegment> = JsonDecode.arraySegment OfJson.Invoke
    
    type OfJson with static member inline OfJson (_: list<'a>, _: OfJson) : JsonValue -> ParseResult<list<'a>> = JsonDecode.list  OfJson.Invoke
    type OfJson with static member inline OfJson (_: 'a Set  , _: OfJson) : JsonValue -> ParseResult<'a Set>   = JsonDecode.set   OfJson.Invoke

    type OfJson with static member inline OfJson (_: Map<string, 'a>, _: OfJson) : JsonValue -> ParseResult<Map<string, 'a>> = JsonDecode.map OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: Dictionary<string, 'a>, _: OfJson) : JsonValue -> ParseResult<Dictionary<string, 'a>> = JsonDecode.dictionary  OfJson.Invoke
        static member inline OfJson (_: ResizeArray<'a>       , _: OfJson) : JsonValue -> ParseResult<ResizeArray<'a>>        = JsonDecode.resizeArray OfJson.Invoke
        static member inline OfJson (_: 'a Id1, _: OfJson) : JsonValue -> ParseResult<Id1<'a>> = fun _ -> Success (Id1<'a> Unchecked.defaultof<'a>)
        static member inline OfJson (_: 'a Id2, _: OfJson) : JsonValue -> ParseResult<Id2<'a>> = fun _ -> Success (Id2<'a> Unchecked.defaultof<'a>)

    type OfJson with
        static member inline OfJson (_: 'a * 'b, _: OfJson) : JsonValue -> ParseResult<'a * 'b> = JsonDecode.tuple2 OfJson.Invoke OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c, _: OfJson) : JsonValue -> ParseResult<'a * 'b * 'c> = JsonDecode.tuple3 OfJson.Invoke OfJson.Invoke OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd, _: OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> = JsonDecode.tuple4 OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e, _: OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> = JsonDecode.tuple5 OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f, _: OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> = JsonDecode.tuple6 OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, _: OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> = JsonDecode.tuple7 OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke OfJson.Invoke

    // Default, for external classes.
    type OfJson with
        static member inline OfJson (_: 'R, _: Default7) =
            let codec = (^R : (static member JsonObjCodec : Codec<IReadOnlyDictionary<string,JsonValue>,'R>) ())
            codec |> Codec.compose jsonObjToValueCodec |> fst : JsonValue -> ^R ParseResult

        static member inline OfJson (_: 'R, _: Default6) =
            let codec = (^R : (static member JsonObjCodec : ConcreteCodec<_,_,_,'R>) ())
            codec |> Codec.ofConcrete |> Codec.compose jsonObjToValueCodec |> fst : JsonValue -> ^R ParseResult

        static member inline OfJson (r: 'R, _: Default5) = Result.catch (Error << DecodeError.Uncategorized) << (^R : (static member FromJSON: ^R  -> (JsonValue -> Result< ^R, string>)) r) : JsonValue ->  ^R ParseResult
        static member inline OfJson (_: 'R, _: Default4) = fun js -> Result.catch (Error << DecodeError.Uncategorized) (^R : (static member OfJson: JsonValue -> Result< ^R, string>) js) : ^R ParseResult
        static member inline OfJson (r: 'R, _: Default3) = (^R : (static member FromJSON: ^R  -> (JsonValue -> ^R ParseResult)) r) : JsonValue ->  ^R ParseResult
        static member inline OfJson (_: 'R, _: Default2) = fun js -> (^R : (static member OfJson: JsonValue -> ^R ParseResult) js) : ^R ParseResult

        static member OfJson (_: JsonObject, _: Default1) = JsonHelpers.jsonObjectOfJson


    /// Maps Json to a type
    let inline ofJson (x: JsonValue) : 't ParseResult = OfJson.Invoke x

    [<Obsolete("Use 'ofJson'")>]
    let inline fromJSON (x: JsonValue) : 't ParseResult = OfJson.Invoke x

    /// Gets a value from a Json object
    let inline jgetWith ofJson (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> ofJson value
        | _ -> Decode.Fail.propertyNotFound key o

    /// Gets a value from a Json object
    let inline jget (o: IReadOnlyDictionary<string, JsonValue>) key = jgetWith ofJson o key

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline jgetOptWith ofJson (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> ofJson value |> map Some
        | _ -> Success None

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline jgetOpt (o: IReadOnlyDictionary<string, JsonValue>) key = jgetOptWith ofJson o key

    [<Obsolete("Use 'jgetOpt'")>]
    let inline jgetopt (o: IReadOnlyDictionary<string, JsonValue>) key = jgetOptWith ofJson o key


    // Serializing:

    type ToJson =
        inherit Default1
        static member ToJson (x: bool          , _: ToJson) = JsonEncode.boolean        x
        static member ToJson (x: string        , _: ToJson) = JsonEncode.string         x
        static member ToJson (x: DateTime      , _: ToJson) = JsonEncode.dateTime       x
        static member ToJson (x: DateTimeOffset, _: ToJson) = JsonEncode.dateTimeOffset x
        static member ToJson (x: decimal       , _: ToJson) = JsonEncode.decimal        x
        static member ToJson (x: Double        , _: ToJson) = JsonEncode.float          x
        static member ToJson (x: Single        , _: ToJson) = JsonEncode.float32        x
        static member ToJson (x: int           , _: ToJson) = JsonEncode.int            x
        static member ToJson (x: uint32        , _: ToJson) = JsonEncode.uint32         x
        static member ToJson (x: int64         , _: ToJson) = JsonEncode.int64          x
        static member ToJson (x: uint64        , _: ToJson) = JsonEncode.uint64         x
        static member ToJson (x: int16         , _: ToJson) = JsonEncode.int16          x
        static member ToJson (x: uint16        , _: ToJson) = JsonEncode.uint16         x
        static member ToJson (x: byte          , _: ToJson) = JsonEncode.byte           x
        static member ToJson (x: sbyte         , _: ToJson) = JsonEncode.sbyte          x
        static member ToJson (x: char          , _: ToJson) = JsonEncode.char           x
        static member ToJson (x: Guid          , _: ToJson) = JsonEncode.guid           x
        static member ToJson (()               , _: ToJson) = JsonEncode.unit ()

    type ToJson with
        static member inline Invoke (x: 't) : JsonValue =
            let inline iToJson (a: ^a, b: ^b) = ((^a or ^b) : (static member ToJson : ^b * _ -> JsonValue) b, a)
            iToJson (Unchecked.defaultof<ToJson>, x)

    type ToJson with
        static member inline ToJson (x: Choice<'a, 'b>, _: ToJson) = JsonEncode.choice ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: Choice<'a, 'b, 'c>, _: ToJson) = JsonEncode.choice3 ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: 'a option, _: ToJson) = JsonEncode.option ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: 'a Nullable, _: ToJson) = JsonEncode.nullable ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: list<'a>, _: ToJson) = JsonEncode.list ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: 'a Set, _: ToJson) = JsonEncode.set ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: 'a array, _: ToJson) = JsonEncode.array ToJson.Invoke x
        static member inline ToJson (x: 'a ArraySegment, _: ToJson) = JsonEncode.arraySegment ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x: Map<string, 'a>, _: ToJson) = JsonEncode.map ToJson.Invoke x

    type ToJson with        
        static member inline ToJson (x: Dictionary<string, 'a>, _: ToJson) = JsonEncode.dictionary  ToJson.Invoke x
        static member inline ToJson (x: 'a ResizeArray        , _: ToJson) = JsonEncode.resizeArray ToJson.Invoke x
        static member inline ToJson (x                        , _: ToJson) = JsonEncode.tuple2      ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x, _: ToJson) = JsonEncode.tuple3 ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x, _: ToJson) = JsonEncode.tuple4 ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x, _: ToJson) = JsonEncode.tuple5 ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x, _: ToJson) = JsonEncode.tuple6 ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    type ToJson with
        static member inline ToJson (x, _: ToJson) = JsonEncode.tuple7 ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke ToJson.Invoke x

    // Default, for external classes.
    type ToJson with
        static member inline ToJson (t: 'T, _: Default5) =
            let codec = (^T : (static member JsonObjCodec : Codec<IReadOnlyDictionary<string,JsonValue>,'T>) ())
            (codec |> Codec.compose jsonObjToValueCodec |> snd) t

        static member inline ToJson (t: 'T, _: Default4) =
            let codec = (^T : (static member JsonObjCodec : ConcreteCodec<_,_,_,'T>) ())
            (codec |> Codec.ofConcrete |> Codec.compose jsonObjToValueCodec |> snd) t

        static member inline ToJson (t: 'T, _: Default3) = (^T : (static member ToJSON : ^T -> JsonValue) t)
        static member inline ToJson (t: 'T, _: Default2) = (^T : (static member ToJson : ^T -> JsonValue) t)

   
    /// Maps a value to Json
    let inline toJson (x: 't) : JsonValue = ToJson.Invoke x

    [<Obsolete("Use 'toJson'")>]
    let inline toJSON (x: 't) : JsonValue = ToJson.Invoke x

    /// Derive automatically a JsonCodec, based of OfJson and ToJson static members
    let inline getJsonValueCodec () : Codec<JsonValue, 't> = ofJson, toJson

    /// Derive automatically a JsonCodec, based of OfJson and ToJson static members
    let inline jsonValueCodec< ^t when (OfJson or ^t) : (static member OfJson : ^t * OfJson -> (JsonValue -> ^t ParseResult)) and (ToJson or ^t) : (static member ToJson : ^t * ToJson -> JsonValue)> : Codec<JsonValue,'t> = ofJson, toJson

    /// Parses a Json Text and maps to a type
    let inline parseJson (x: string) : 'a ParseResult = fst jsonValueToTextCodec x >>= ofJson

    [<Obsolete("Use 'parseJson'")>]
    let inline parseJSON (x: string) : 'a ParseResult = parseJson (x: string)



    /// Creates a new Json key,value pair for a Json object
    let inline jpairWith toJson (key: string) value = key, toJson value

    /// Creates a new Json key,value pair for a Json object
    let inline jpair (key: string) value = jpairWith toJson key value
    
    /// Creates a new Json key,value pair for a Json object if the value option is present
    let inline jpairOptWith toJson (key: string) value = match value with Some value -> (key, toJson value) | _ -> (null, JNull)

    /// Creates a new Json key,value pair for a Json object if the value option is present
    let inline jpairOpt (key: string) value = jpairOptWith toJson key value

    /// <summary>Initialize the field mappings.</summary>
    /// <param name="f">An object constructor as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let withFields f = (fun _ -> Success f), (fun _ -> rdict [])

    let diApply combiner (remainderFields: SplitCodec<'S, 'f ->'r, 'T>) (currentField: SplitCodec<'S, 'f, 'T>) =
        ( 
            Compose.run (Compose (fst remainderFields: Decoder<'S, 'f -> 'r>) <*> Compose (fst currentField)),
            fun p -> combiner (snd remainderFields p) ((snd currentField) p)
        )

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldWith codec fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _ -> 'Rest, _>) =
        let inline deriveFieldCodec codec prop getter =
            (
                (fun (o: IReadOnlyDictionary<string,JsonValue>) -> jgetWith (fst codec) o prop),
                (getter >> fun (x: 'Value) -> rdict [prop, (snd codec) x])
            )
        diApply IReadOnlyDictionary.union rest (deriveFieldCodec codec fieldName getter)

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfield fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _ -> 'Rest, _>) = jfieldWith jsonValueCodec fieldName getter rest

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldOptWith codec fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _ -> 'Rest, _>) =
        let inline deriveFieldCodecOpt codec prop getter =
            (
                (fun (o: IReadOnlyDictionary<string,JsonValue>) -> jgetOptWith (fst codec) o prop),
                (getter >> function Some (x: 'Value) -> rdict [prop, (snd codec) x] | _ -> rdict [])
            )
        diApply IReadOnlyDictionary.union rest (deriveFieldCodecOpt codec fieldName getter)

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldOpt fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _ -> 'Rest, _>) = jfieldOptWith jsonValueCodec fieldName getter rest
    
  
    module Operators =

        /// Creates a new Json key,value pair for a Json object
        let inline (.=) key value = jpair key value
        
        /// Creates a new Json key,value pair for a Json object if the value is present in the option
        let inline (.=?) (key: string) value = jpairOpt key value

        /// Gets a value from a Json object
        let inline (.@) o key = jget o key

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let inline (.@?) o key = jgetOpt o key
        
        /// <summary>Applies a field mapping to the object codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<*/>) (rest: SplitCodec<_, _->'Rest, _>) (fieldName, getter: 'T -> 'Value) = jfield fieldName getter rest

        /// <summary>Appends the first field mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="f">An object initializer as a curried function.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<!/>) f (fieldName, getter: 'T -> 'Value) = jfield fieldName getter (withFields f)

        /// <summary>Appends an optional field mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<*/?>) (rest: SplitCodec<_, _ -> 'Rest, _>) (fieldName, getter: 'T -> 'Value option) = jfieldOpt fieldName getter rest

        /// <summary>Appends the first field (optional) mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="f">An object initializer as a curried function.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<!/?>) f (fieldName, getter: 'T -> 'Value option) = jfieldOpt fieldName getter (withFields f)

        /// Tuple two values.
        let inline (^=) a b = (a, b)

        /// Gets a value from a Json object
        let inline jgetFromListWith ofJson (o: list<KeyValuePair<string, JsonValue>>) key =
            match List.tryFind (fun (KeyValue(x, _)) -> x = key) o with
            | Some (KeyValue(_, value)) -> ofJson value
            | _                         -> Decode.Fail.propertyNotFound key (ofList o)

        // /// Gets a value from a Json object
        // let inline jgetFromList (o: list<KeyValuePair<string, JsonValue>>) key = jgetFromListWith ofJson o key

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let inline jgetFromListOptWith ofJson (o: list<KeyValuePair<string, JsonValue>>) key =
            match List.tryFind (fun (KeyValue(x, _)) -> x = key) o with
            | Some (KeyValue(_, value)) -> ofJson value |> map Some
            | _ -> Success None

        let inline joptWith codec prop getter =
            {
                Decoder = ReaderT (fun (o: list<KeyValuePair<string, JsonValue>>) -> jgetFromListOptWith (fst codec) o prop)
                Encoder = fun x -> Const (match getter x with Some (x: 'Value) -> [KeyValuePair (prop, (snd codec) x)] | _ -> [])
            }

        /// Derives a concrete field codec for an optional field
        let inline jopt prop getter = joptWith jsonValueCodec prop getter

        let inline jreqWith codec (prop: string) (getter: 'T -> 'Value option) =
            {
                Decoder = ReaderT (fun (o: list<KeyValuePair<string, JsonValue>>) -> jgetFromListWith (fst codec) o prop)
                Encoder = fun x -> Const (match getter x with Some (x: 'Value) -> [KeyValuePair (prop, (snd codec) x)] | _ -> [])
            }

        /// Derives a concrete field codec for a required field
        let inline jreq (name: string) (getter: 'T -> 'param option) = jreqWith jsonValueCodec name getter

        let inline jchoice (codecs: seq<ConcreteCodec<'S, 'S, 't1, 't2>>) =
            let head, tail = Seq.head codecs, Seq.tail codecs
            foldBack (<|>) tail head

    module Lens =
        open FSharpPlus.Lens
        let inline _JString x = (prism' JString <| function JString s -> Some s | _ -> None) x
        let inline _JObject x = (prism' JObject <| function JObject s -> Some s | _ -> None) x
        let inline _JArray  x = (prism' JArray  <| function JArray  s -> Some s | _ -> None) x
        let inline _JBool   x = (prism' JBool   <| function JBool   s -> Some s | _ -> None) x
        let inline _JNumber x = (prism' JNumber <| fun v -> match ofJson v : decimal ParseResult with Ok s -> Some s | _ -> None) x
        let inline _JNull   x = prism' (konst JNull) (function JNull -> Some () | _ -> None) x
        /// Like '_jnth', but for 'Object' with Text indices.
        let inline _jkey i =
            let inline dkey i f t = map (fun x -> IReadOnlyDictionary.add i x t) (f (IReadOnlyDictionary.tryGetValue i t |> Option.defaultValue JNull))
            _JObject << dkey i
        let inline _jnth i =
            let inline dnth i f t = map (fun x -> t |> IReadOnlyList.add i x |> Option.defaultValue t) (f (IReadOnlyList.tryItem i t |> Option.defaultValue JNull))
            _JArray << dnth i

        // Reimport some basic Lens operations from F#+

        let setl optic value   (source: 's) : 't = setl optic value source
        let over optic updater (source: 's) : 't = over optic updater source
        let preview (optic: ('a -> Const<_,'b>) -> _ -> Const<_,'t>) (source: 's) : 'a option = preview optic source