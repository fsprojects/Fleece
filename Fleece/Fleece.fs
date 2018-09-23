namespace Fleece

open FSharpPlus
#if NEWTONSOFT
module Newtonsoft =
#endif
#if FSHARPDATA
module FSharpData =
#endif
#if SYSTEMJSON
[<AutoOpen>]
module Fleece =
#endif
    open System
    open System.Globalization    
    open System.Collections.Generic
    open FSharpPlus
    open FSharpPlus.Data
    module ReadOnlyCollections=
        open System.Collections.ObjectModel
        type IDictionary<'key, 'value> with
            member self.AsReadOnlyDictionary() = ReadOnlyDictionary(self) :> IReadOnlyDictionary<_,_>
        type IList<'value> with
            member self.AsReadOnlyList() = ReadOnlyCollection(self) :> IReadOnlyList<_>
        type IEnumerable<'value> with
            member self.ToReadOnlyList() = ResizeArray(self).AsReadOnlyList()
    open ReadOnlyCollections
    module ReadOnlyList=
        let ofArray (a:_ array) = a.AsReadOnlyList()
        let toArray (a:IReadOnlyList<_>) = a |> Array.ofSeq
        // add has same shape as add for Map.add
        /// Returns a new IReadOnlyList from a given IReadOnlyList, with replaced binding for index.
        let add i value (a:IReadOnlyList<_>)=
            let setNth i v (a:_ array) = a.[i] <- v; a
            if 0<=i && i<a.Count then
                a |> Array.ofSeq |> setNth i value |> ofArray |> Some
            else
                None
        let tryNth i (a:IReadOnlyList<_>)=
            if 0<=i && i<a.Count then
                Some a.[i]
            else
                None

    type Id1<'t>(v:'t) =
        let value = v
        member __.getValue = value

    type Id2<'t>(v:'t) =
        let value = v
        member __.getValue = value

    type Default4 = class end
    type Default3 = class inherit Default4 end
    type Default2 = class inherit Default3 end
    type Default1 = class inherit Default2 end

    #if NEWTONSOFT
    
    open Newtonsoft.Json.Linq
    type JsonValue = JToken
    type JObject with
        member x.AsReadOnlyDictionary() =
            (x.Properties() |> Seq.map ( fun p-> (p.Name,p.Value) ) |> dict).AsReadOnlyDictionary()

        static member GetValues (x: JObject) = x.AsReadOnlyDictionary()

    let jsonObjectGetValues (x : JObject) = JObject.GetValues x

    type JsonObject = JObject
            
    
    type private JsonHelpers() =
        static member create (x: decimal) = JValue          x  :> JToken
        static member create (x: Double ) = JValue          x  :> JToken
        static member create (x: Single ) = JValue (float   x) :> JToken
        static member create (x: int    ) = JValue (decimal x) :> JToken
        static member create (x: bool   ) = JValue          x  :> JToken
        static member create (x: uint32 ) = JValue (decimal x) :> JToken
        static member create (x: int64  ) = JValue (decimal x) :> JToken
        static member create (x: uint64 ) = JValue (decimal x) :> JToken
        static member create (x: int16  ) = JValue (decimal x) :> JToken
        static member create (x: uint16 ) = JValue (decimal x) :> JToken
        static member create (x: byte   ) = JValue (decimal x) :> JToken
        static member create (x: sbyte  ) = JValue (decimal x) :> JToken
        static member create (x: char   ) = JValue (string  x) :> JToken
        static member create (x: Guid   ) = JValue (string  x) :> JToken


    // FSharp.Newtonsoft.Json AST adapter

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o:JToken) =
        match o.Type with
        | JTokenType.Null    -> JNull
        | JTokenType.Array   -> JArray ((o :?> JArray).AsReadOnlyList ())
        | JTokenType.Object  -> JObject (jsonObjectGetValues (o :?> JObject))
        | JTokenType.Integer -> JNumber  o
        | JTokenType.Float   -> JNumber  o
        | JTokenType.Boolean -> JBool   (o.ToObject () : bool)
        | JTokenType.String  -> JString (o.ToObject () : string)
        | t                  -> failwithf "Invalid JTokenType %A" t
    
    let dictAsProps (x: IReadOnlyDictionary<string, JToken>) = 
        x |> Seq.map (|KeyValue|) |> Array.ofSeq 

    let inline JArray (x: JToken IReadOnlyList) = JArray (x |> Array.ofSeq) :> JToken
    let inline JObject (x: IReadOnlyDictionary<string, JToken>) 
        =
        let o = JObject()
        for kv in x do
            o.Add(kv.Key, kv.Value)
        o :> JToken
    let inline JBool (x: bool) = JValue x :> JToken
    let inline JNumber (x: decimal) = JValue x :> JToken
    let JNull = JValue.CreateNull() :> JToken
    let inline JString (x: string) = if isNull x then JNull else JValue x :> JToken
    
    #endif
    #if FSHARPDATA
    
    open FSharp.Data
        
    type JsonObject = (string * JsonValue)[]
            
    
    type private JsonHelpers() =
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


    type private ReadOnlyJsonPropertiesDictionary(properties:(string * JsonValue)[]) =                
        
        let properties = properties

        member __.Properties = properties

        with
            interface System.Collections.IEnumerable with
                member __.GetEnumerator() = (properties |> Seq.map KeyValuePair).GetEnumerator() :> System.Collections.IEnumerator

            interface IEnumerable<KeyValuePair<string, JsonValue>> with
                member __.GetEnumerator() = (properties |> Seq.map KeyValuePair).GetEnumerator()

            interface IReadOnlyCollection<KeyValuePair<string,JsonValue>> with
                member __.Count = properties.Length
        
            interface IReadOnlyDictionary<string, JsonValue> with
                member __.Keys = properties |> Seq.map fst                
                member __.Values = properties |> Seq.map snd                
                member __.Item with get(key:string) = properties |> Array.find (fun (k,_) -> k = key) |> snd                
                member __.ContainsKey(key:string) = properties |> Array.exists (fun (k,_) -> k = key)                
                member __.TryGetValue(key:string, value:byref<JsonValue>) =
                    match properties |> Array.tryFindIndex (fun (k,_) -> k = key) with
                    | Some i -> 
                        value <- snd properties.[i]
                        true
                    | None -> false                                


    let jsonObjectGetValues (x : JsonObject) = ReadOnlyJsonPropertiesDictionary(x) :> IReadOnlyDictionary<string, JsonValue>


    // FSharp.Data.JsonValue AST adapter

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o:JsonValue) =
        match o with
        | JsonValue.Null          -> JNull
        | JsonValue.Array els     -> JArray (els.AsReadOnlyList ())
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
    #if SYSTEMJSON
    
    open System.Json

    type JsonObject with
        member x.AsReadOnlyDictionary() =
            (x :> IDictionary<string, JsonValue>).AsReadOnlyDictionary()

        static member GetValues (x: JsonObject) = x.AsReadOnlyDictionary()

    let jsonObjectGetValues (x : JsonObject) = JsonObject.GetValues x


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
        static member create (x: char   ) = JsonPrimitive x :> JsonValue
        static member create (x: Guid   ) = JsonPrimitive x :> JsonValue


    // pseudo-AST, wrapping JsonValue subtypes:

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonValue) =
        match o with
        | null -> JNull
        | :? JsonArray as x ->
            let values = (x :> JsonValue IList).AsReadOnlyList()
            JArray values
        | :? JsonObject as x ->
            JObject (x.AsReadOnlyDictionary())
        | :? JsonPrimitive as x ->
            match x.JsonType with
            | JsonType.Number  -> JNumber x
            | JsonType.Boolean -> JBool (x.ReadAs<bool> ())
            | JsonType.String  -> JString (x.ReadAs<string> ())
            | JsonType.Default -> JNull
            | _ -> failwithf "Invalid JsonType %A for primitive %A" x.JsonType x
        | _ -> failwithf "Invalid JsonValue %A" o

    let inline JArray (x: JsonValue IReadOnlyList) = JsonArray x :> JsonValue
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonObject x :> JsonValue
    let inline JBool (x: bool) = JsonPrimitive x :> JsonValue
    let JNull : JsonValue = null
    let inline JString (x: string) = if isNull x then JNull else JsonPrimitive x :> JsonValue
    let inline JNumber (x: decimal) = JsonPrimitive x :> JsonValue
    #endif


    // results:

    let (|Success|Failure|) =
        function
        | Ok    x -> Success x
        | Error x -> Failure x

    let inline Success x = Ok    x
    let inline Failure x = Error x

    // Deserializing:

    type 'a ParseResult = Result<'a, string>

    module Helpers =

        let inline failparse s v = Failure (sprintf "Expected %s, actual %A" s v)

        let listAsReadOnly (l: _ list) =
            { new IReadOnlyList<_> with
                member __.Count = l.Length
                member __.Item with get index = l.[index]
                member __.GetEnumerator() = (l :> _ seq).GetEnumerator()
                member __.GetEnumerator() = (l :> System.Collections.IEnumerable).GetEnumerator() }

        let dict x = (dict x).AsReadOnlyDictionary()

        let keys   (x: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(k,_)) -> k) x
        let values (x: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(_,v)) -> v) x


        #if NEWTONSOFT

        let inline tryRead<'a> s = 
            function
            | JNumber j -> 
                try
                  Success (j.ToObject<'a>())
                with 
                | _ -> failparse s j
            | a -> failparse s a

        type JsonHelpers with        
            static member jsonObjectOfJson =
                fun (o: JToken) ->
                    match o.Type with
                    | JTokenType.Object -> Success ( o :?> JObject )
                    | a -> failparse "JsonObject" a
 

        #endif
        #if FSHARPDATA

        let inline tryRead s = 
            function
            | JsonValue.Number n -> Success (explicit n)
            | JsonValue.Float  n -> Success (explicit n)
            | js                 -> failparse s (sprintf "Expected numeric but was %A" js)

        type JsonHelpers with
            static member jsonObjectOfJson =
                fun (o: JsonValue) ->
                    match o with
                    | JObject x -> Success (dictAsProps x)
                    | a -> failparse "JsonObject" a


        #endif
        #if SYSTEMJSON

        let inline tryRead<'a> s = 
            function
            | JNumber j -> 
                match j.TryReadAs<'a>() with
                | true, v -> Success v
                | _ -> failparse s j
            | a -> failparse s a

        type JsonHelpers with
            static member inline jsonObjectOfJson =
                fun (o: JsonValue) ->
                    match box o with
                    | :? JsonObject as x -> Success x
                    | a -> failparse "JsonObject" a

        #endif

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

    let decode (d: Decoder<'i, 'a>) (i: 'i) : ParseResult<'a> = d i
    let encode (e: Encoder<'o, 'a>) (a: 'a) : 'o = e a


    // Deserializing:

    open Helpers

    type Decimal with static member OfJson x = tryRead<decimal> "decimal" x
    type Int16   with static member OfJson x = tryRead<int16>   "int16"   x
    type Int32   with static member OfJson x = tryRead<int>     "int"     x
    type Int64   with static member OfJson x = tryRead<int64>   "int64"   x
    type UInt16  with static member OfJson x = tryRead<uint16>  "uint16"  x
    type UInt32  with static member OfJson x = tryRead<uint32>  "uint32"  x
    type UInt64  with static member OfJson x = tryRead<uint64>  "uint64"  x
    type Byte    with static member OfJson x = tryRead<byte>    "byte"    x
    type SByte   with static member OfJson x = tryRead<sbyte>   "sbyte"   x
    type Double  with static member OfJson x = tryRead<double>  "double"  x
    type Single  with static member OfJson x = tryRead<single>  "single"  x    

    type Boolean with static member OfJson x =
                                match x with
                                | JBool b -> Success b
                                | a -> failparse "bool" a

    type String with static member OfJson x =
                                match x with
                                | JString b -> Success b
                                | JNull -> Success null
                                | a -> failparse "string" a

    type Char with static member OfJson x =
                                match x with
                                | JString null -> Failure "Expected char, got null"
                                | JString s    -> Success s.[0]
                                | a -> failparse "char" a

    type Guid with static member OfJson x =
                                match x with
                                | JString null -> Failure "Expected Guid, got null"
                                | JString s    -> tryParse<Guid> s |> option Success (Failure ("Invalid Guid " + s))
                                | a -> failparse "Guid" a

    type DateTime with static member OfJson x =
                                match x with
                                | JString null -> Failure "Expected DateTime, got null"
                                | JString s    ->
                                    match DateTime.TryParseExact(s, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
                                    | true, t -> Success t
                                    | _       -> Failure (sprintf "Invalid DateTime %s" s)
                                | a -> failparse "DateTime" a

    type DateTimeOffset with static member OfJson x =
                                match x with
                                | JString null -> Failure "Expected DateTimeOffset, got null"
                                | JString s    ->
                                    match DateTimeOffset.TryParseExact(s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
                                    | true, t -> Success t
                                    | _       -> Failure (sprintf "Invalid DateTimeOffset %s" s)
                                | a -> failparse "DateTimeOffset" a

    type OfJson =
        inherit Default1
        
        static member OfJson (_: decimal, _: OfJson) = Decimal.OfJson
        static member OfJson (_: int16  , _: OfJson) = Int16  .OfJson
        static member OfJson (_: int    , _: OfJson) = Int32  .OfJson
        static member OfJson (_: int64  , _: OfJson) = Int64  .OfJson
        static member OfJson (_: uint16 , _: OfJson) = UInt16 .OfJson
        static member OfJson (_: uint32 , _: OfJson) = UInt32 .OfJson
        static member OfJson (_: uint64 , _: OfJson) = UInt64 .OfJson
        static member OfJson (_: byte   , _: OfJson) = Byte   .OfJson
        static member OfJson (_: sbyte  , _: OfJson) = SByte  .OfJson
        static member OfJson (_: double , _: OfJson) = Double .OfJson
        static member OfJson (_: single , _: OfJson) = Single .OfJson

        static member OfJson (_: bool          , _: OfJson) = Boolean       .OfJson
        static member OfJson (_: string        , _: OfJson) = String        .OfJson
        static member OfJson (_: char          , _: OfJson) = Char          .OfJson
        static member OfJson (_: Guid          , _: OfJson) = Guid          .OfJson
        static member OfJson (_: DateTime      , _: OfJson) = DateTime      .OfJson
        static member OfJson (_: DateTimeOffset, _: OfJson) = DateTimeOffset.OfJson

    type OfJson with
        static member inline Invoke (x: JsonValue) : 't ParseResult =
            let inline iOfJson (a: ^a, b: ^b) = ((^a or ^b) : (static member OfJson: ^b * _ -> (JsonValue -> ^b ParseResult)) b, a)
            iOfJson (Unchecked.defaultof<OfJson>, Unchecked.defaultof<'t>) x

    type OfJson with
        static member inline OfJson (_: Choice<'a, 'b>, _:OfJson) : JsonValue -> ParseResult<Choice<'a, 'b>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> OfJson.Invoke |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> OfJson.Invoke |> map Choice2Of2
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type OfJson with
        static member inline OfJson (_: Choice<'a, 'b, 'c>, _:OfJson) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> OfJson.Invoke |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> OfJson.Invoke |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> OfJson.Invoke |> map Choice3Of3
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type OfJson with
        static member inline OfJson (_: 'a option, _:OfJson) : JsonValue -> ParseResult<'a option> =
            function
            | JNull _ -> Success None
            | x -> 
                let a: 'a ParseResult = OfJson.Invoke x
                map Some a

    type OfJson with
        static member inline OfJson (_: 'a Nullable, _:OfJson) : JsonValue -> ParseResult<'a Nullable> =
            function
            | JNull _ -> Success (Nullable())
            | x -> 
                let a: 'a ParseResult = OfJson.Invoke x
                map (fun x -> Nullable x) a

    type OfJson with
        static member inline OfJson (_: 'a array, _:OfJson) : JsonValue -> ParseResult<'a array> =
            function
            | JArray a -> traverse OfJson.Invoke a |> map Seq.toArray
            | a -> failparse "array" a

    type OfJson with
        static member inline OfJson (_: list<'a>, _:OfJson) : JsonValue -> ParseResult<list<'a>> =
            function
            | JArray a -> traverse OfJson.Invoke a |> map Seq.toList
            | a -> failparse "array" a

    type OfJson with
        static member inline OfJson (_: 'a Set, _:OfJson) : JsonValue -> ParseResult<'a Set> =
            function
            | JArray a -> traverse OfJson.Invoke a |> map set
            | a -> failparse "array" a

    type OfJson with
        static member inline OfJson (_: Map<string, 'a>, _:OfJson) : JsonValue -> ParseResult<Map<string, 'a>> =
            function
            | JObject o -> traverse OfJson.Invoke (values o) |> map (fun values -> Seq.zip (keys o) values |> Map.ofSeq)
            | a -> failparse "Map" a

    type OfJson with
        static member inline OfJson (_: Dictionary<string, 'a>, _:OfJson) : JsonValue -> ParseResult<Dictionary<string, 'a>> =
            function
            | JObject o -> traverse OfJson.Invoke (values o) |> map (fun values -> Seq.zip (keys o) values |> ofSeq)
            | a -> failparse "Dictionary" a

        static member inline OfJson (_: 'a ResizeArray, _:OfJson) : JsonValue -> ParseResult<'a ResizeArray> =
            function
            | JArray a -> traverse OfJson.Invoke a |> map (fun x -> ResizeArray<_>(x: 'a seq))
            | a -> failparse "ResizeArray" a

        static member inline OfJson (_: 'a Id1, _:OfJson) : JsonValue -> ParseResult<Id1<'a>> = fun _ -> Success (Id1<'a>(Unchecked.defaultof<'a>))
        static member inline OfJson (_: 'a Id2, _:OfJson) : JsonValue -> ParseResult<Id2<'a>> = fun _ -> Success (Id2<'a>(Unchecked.defaultof<'a>))

    type OfJson with
        static member inline OfJson (_: 'a * 'b, _:OfJson) : JsonValue -> ParseResult<'a * 'b> =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + string x)
                else
                    tuple2 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c, _:OfJson) : JsonValue -> ParseResult<'a * 'b * 'c> =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + string x)
                else
                    tuple3 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1] <*> OfJson.Invoke a.[2]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd, _:OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + string x)
                else
                    tuple4 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1] <*> OfJson.Invoke a.[2] <*> OfJson.Invoke a.[3]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e, _:OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + string x)
                else
                    tuple5 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1] <*> OfJson.Invoke a.[2] <*> OfJson.Invoke a.[3] <*> OfJson.Invoke a.[4]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f, _:OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + string x)
                else
                    tuple6 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1] <*> OfJson.Invoke a.[2] <*> OfJson.Invoke a.[3] <*> OfJson.Invoke a.[4] <*> OfJson.Invoke a.[5]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJson with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, _:OfJson) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + string x)
                else
                    tuple7 <!> OfJson.Invoke a.[0] <*> OfJson.Invoke a.[1] <*> OfJson.Invoke a.[2] <*> OfJson.Invoke a.[3] <*> OfJson.Invoke a.[4] <*> OfJson.Invoke a.[5] <*> OfJson.Invoke a.[6]
            | a -> Failure (sprintf "Expected array, found %A" a)

    // Default, for external classes.
    type OfJson with 
        static member inline OfJson (_: 'R, _:Default4) =
            let codec = (^R : (static member JsonObjCodec: Codec<IReadOnlyDictionary<string,JsonValue>,'R>) ())
            function
            | JObject o -> decode (fst codec) o : ^R ParseResult
            | a         -> failparse "Map" a

        static member inline OfJson (r: 'R, _:Default3) = (^R : (static member FromJSON: ^R  -> (JsonValue -> ^R ParseResult)) r) : JsonValue ->  ^R ParseResult
        static member inline OfJson (_: 'R, _:Default2) = fun js -> (^R : (static member OfJson: JsonValue -> ^R ParseResult) js) : ^R ParseResult

        static member OfJson (_:JsonObject, _:Default1) = JsonHelpers.jsonObjectOfJson        


    /// Maps Json to a type
    let inline ofJson (x: JsonValue) : 't ParseResult = OfJson.Invoke x

    [<Obsolete("Use 'ofJson'")>]
    let inline fromJSON (x: JsonValue) : 't ParseResult = OfJson.Invoke x

    /// Parses Json and maps to a type
    let inline parseJson (x: string) : 'a ParseResult =
        try
            let json = JsonValue.Parse x
            ofJson json
        with e -> Failure (e.ToString())

    [<Obsolete("Use 'parseJson'")>]
    let inline parseJSON (x: string) : 'a ParseResult = parseJson (x: string)

    /// Gets a value from a Json object
    let inline jget (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> ofJson value
        | _ -> Failure ("Key '" + key + "' not found in " + JObject(o).ToString())

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline jgetopt (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> ofJson value |> map Some
        | _ -> Success None


    // Serializing:

    /// Creates a new Json object for serialization
    let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> dict)

    type Boolean        with static member ToJson (x: bool          ) = JBool x
    type String         with static member ToJson (x: string        ) = JString x
    type DateTime       with static member ToJson (x: DateTime      ) = JString (x.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")) // JsonPrimitive is incorrect for DateTime
    type DateTimeOffset with static member ToJson (x: DateTimeOffset) = JString (x.ToString("yyyy-MM-ddTHH:mm:ss.fffK")) // JsonPrimitive is incorrect for DateTimeOffset
    type Decimal        with static member ToJson (x: decimal       ) = JsonHelpers.create x
    type Double         with static member ToJson (x: Double        ) = JsonHelpers.create x
    type Single         with static member ToJson (x: Single        ) = JsonHelpers.create x
    type Int32          with static member ToJson (x: int           ) = JsonHelpers.create x
    type UInt32         with static member ToJson (x: uint32        ) = JsonHelpers.create x
    type Int64          with static member ToJson (x: int64         ) = JsonHelpers.create x
    type UInt64         with static member ToJson (x: uint64        ) = JsonHelpers.create x
    type Int16          with static member ToJson (x: int16         ) = JsonHelpers.create x
    type UInt16         with static member ToJson (x: uint16        ) = JsonHelpers.create x
    type Byte           with static member ToJson (x: byte          ) = JsonHelpers.create x
    type SByte          with static member ToJson (x: sbyte         ) = JsonHelpers.create x
    type Char           with static member ToJson (x: char          ) = JsonHelpers.create x
    type Guid           with static member ToJson (x: Guid          ) = JsonHelpers.create x


    type ToJson =
        inherit Default1
        static member ToJson (x: bool          , _:ToJson) = Boolean       .ToJson x
        static member ToJson (x: string        , _:ToJson) = String        .ToJson x
        static member ToJson (x: DateTime      , _:ToJson) = DateTime      .ToJson x
        static member ToJson (x: DateTimeOffset, _:ToJson) = DateTimeOffset.ToJson x
        static member ToJson (x: decimal       , _:ToJson) = Decimal       .ToJson x
        static member ToJson (x: Double        , _:ToJson) = Double        .ToJson x
        static member ToJson (x: Single        , _:ToJson) = Single        .ToJson x
        static member ToJson (x: int           , _:ToJson) = Int32         .ToJson x
        static member ToJson (x: uint32        , _:ToJson) = UInt32        .ToJson x
        static member ToJson (x: int64         , _:ToJson) = Int64         .ToJson x
        static member ToJson (x: uint64        , _:ToJson) = UInt64        .ToJson x
        static member ToJson (x: int16         , _:ToJson) = Int16         .ToJson x
        static member ToJson (x: uint16        , _:ToJson) = UInt16        .ToJson x
        static member ToJson (x: byte          , _:ToJson) = Byte          .ToJson x
        static member ToJson (x: sbyte         , _:ToJson) = SByte         .ToJson x
        static member ToJson (x: char          , _:ToJson) = Char          .ToJson x
        static member ToJson (x: Guid          , _:ToJson) = Guid          .ToJson x

    type ToJson with
        static member inline Invoke (x: 't) : JsonValue =
            let inline iToJson (a: ^a, b: ^b) = ((^a or ^b) : (static member ToJson: ^b * _ -> JsonValue) b, a)
            iToJson (Unchecked.defaultof<ToJson>, x)

    type ToJson with
        static member inline ToJson (x: Choice<'a, 'b>, _:ToJson) =
            match x with
            | Choice1Of2 a -> jobj [ "Choice1Of2", ToJson.Invoke a ]
            | Choice2Of2 a -> jobj [ "Choice2Of2", ToJson.Invoke a ]

    type ToJson with
        static member inline ToJson (x: Choice<'a, 'b, 'c>, _:ToJson) =
            match x with
            | Choice1Of3 a -> jobj [ "Choice1Of3", ToJson.Invoke a]
            | Choice2Of3 a -> jobj [ "Choice2Of3", ToJson.Invoke a]
            | Choice3Of3 a -> jobj [ "Choice3Of3", ToJson.Invoke a]

    type ToJson with
        static member inline ToJson (x: 'a option, _:ToJson) =
            match x with
            | None -> JNull
            | Some a -> ToJson.Invoke a

    type ToJson with
        static member inline ToJson (x: 'a Nullable, _:ToJson) =
            if x.HasValue 
                then ToJson.Invoke x.Value
                else JNull

    type ToJson with
        static member inline ToJson (x: list<'a>, _:ToJson) =
            JArray (listAsReadOnly (List.map ToJson.Invoke x))

    type ToJson with
        static member inline ToJson (x: 'a Set, _:ToJson) =
            JArray ((Seq.map ToJson.Invoke x).ToReadOnlyList())

    type ToJson with
        static member inline ToJson (x: 'a array, _:ToJson) =
            JArray ((Array.map ToJson.Invoke x).AsReadOnlyList())

    type ToJson with
        static member inline ToJson (x: Map<string, 'a>, _:ToJson) =
            x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k,v)) -> k, ToJson.Invoke v) |> dict |> JObject

    type ToJson with
        static member inline ToJson (x: Dictionary<string, 'a>, _:ToJson) =
            x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k,v)) -> k, ToJson.Invoke v) |> dict |> JObject

        static member inline ToJson (x: 'a ResizeArray, _:ToJson) =
            JArray ((Seq.map ToJson.Invoke x).ToReadOnlyList())

        static member inline ToJson ((a, b), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b|].AsReadOnlyList())

    type ToJson with
        static member inline ToJson ((a, b, c), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b; ToJson.Invoke c|].AsReadOnlyList())

    type ToJson with
        static member inline ToJson ((a, b, c, d), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b; ToJson.Invoke c; ToJson.Invoke d|].AsReadOnlyList())

    type ToJson with
        static member inline ToJson ((a, b, c, d, e), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b; ToJson.Invoke c; ToJson.Invoke d; ToJson.Invoke e|].AsReadOnlyList())

    type ToJson with
        static member inline ToJson ((a, b, c, d, e, f), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b; ToJson.Invoke c; ToJson.Invoke d; ToJson.Invoke e; ToJson.Invoke f|].AsReadOnlyList())

    type ToJson with
        static member inline ToJson ((a, b, c, d, e, f, g), _:ToJson) =
            JArray ([|ToJson.Invoke a; ToJson.Invoke b; ToJson.Invoke c; ToJson.Invoke d; ToJson.Invoke e; ToJson.Invoke f; ToJson.Invoke g|].AsReadOnlyList())

    // Default, for external classes.
    type ToJson with
        static member inline ToJson (t: 'T, _:Default4) = 
            let codec = (^T : (static member JsonObjCodec: Codec<IReadOnlyDictionary<string,JsonValue>,'T>) ())
            JObject (encode (snd codec) t)

        static member inline ToJson (t: 'T, _:Default3) = (^T : (static member ToJSON: ^T -> JsonValue) t)
        static member inline ToJson (t: 'T, _:Default2) = (^T : (static member ToJson: ^T -> JsonValue) t)

   
    /// Maps a value to Json
    let inline toJson (x: 't) : JsonValue = ToJson.Invoke x

    [<Obsolete("Use 'toJson'")>]
    let inline toJSON (x: 't) : JsonValue = ToJson.Invoke x

    /// Creates a new Json key,value pair for a Json object
    let inline jpair (key: string) value = key, toJson value
    
    /// Creates a new Json key,value pair for a Json object if the value option is present
    let inline jpairopt (key: string) value = match value with Some value -> (key, toJson value) | _ -> (null, JNull)


    /// <summary>Initialize the field mappings.</summary>
    /// <param name="f">An object initializer as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let mapping f = (fun _ -> Success f), (fun _ -> dict [])

    let diApply combiner toBC (remainderFields: SplitCodec<'S, 'f ->'r, 'T>) (currentField: Codec<'S, 'f>) =
        ( 
            Compose.run (Compose (fst remainderFields: Decoder<'S, 'f -> 'r>) <*> Compose (fst currentField)),
            toBC >> (encode (snd currentField) *** encode (snd remainderFields)) >> combiner
        )

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfield fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _->'Rest, _>) =
        let inline deriveFieldCodec prop =
            (
                (fun (o: IReadOnlyDictionary<string,JsonValue>) -> jget o prop),
                (fun (x: 't) -> dict [prop, toJson x])
            )
        diApply (IReadOnlyDictionary.union |> flip |> uncurry) (fanout getter id) rest (deriveFieldCodec fieldName)

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldopt fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _->'Rest, _>) =
        let inline deriveFieldCodecOpt prop =
            (
                (fun (o: IReadOnlyDictionary<string,JsonValue>) -> jgetopt o prop),
                (function Some (x: 't) -> dict [prop, toJson x] | _ -> dict [])
            )
        diApply (IReadOnlyDictionary.union |> flip |> uncurry) (fanout getter id) rest (deriveFieldCodecOpt fieldName)

    let inline getCodec () : Codec<JsonValue, 't> = ofJson, toJson
   
    module Operators =

        /// Creates a new Json key,value pair for a Json object
        let inline (.=) key value = jpair key value
        
        /// Creates a new Json key,value pair for a Json object if the value is present in the option
        let inline (.=?) (key: string) value = jpairopt key value

        /// Gets a value from a Json object
        let inline (.@) o key = jget o key

        /// Tries to get a value from a Json object.
        /// Returns None if key is not present in the object.
        let inline (.@?) o key = jgetopt o key
        
        /// <summary>Appends a field mapping to the codec.</summary>
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
        let inline (<!/>) f (fieldName, getter: 'T -> 'Value) = jfield fieldName getter (mapping f)

        /// <summary>Appends an optional field mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="rest">The other mappings.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<*/?>) (rest: SplitCodec<_, _->'Rest, _>) (fieldName, getter: 'T -> 'Value option) = jfieldopt fieldName getter rest

        /// <summary>Appends the first field (optional) mapping to the codec.</summary>
        /// <param name="fieldName">A string that will be used as key to the field.</param>
        /// <param name="getter">The field getter function.</param>
        /// <param name="f">An object initializer as a curried function.</param>
        /// <returns>The resulting object codec.</returns>
        let inline (<!/?>) f (fieldName, getter: 'T -> 'Value option) = jfieldopt fieldName getter (mapping f)

        /// Tuple two values.
        let inline (^=) a b = (a, b)

    module Lens =
        open FSharpPlus.Lens
        let inline _JString x = (prism' JString <| function JString s -> Some s | _ -> None) x
        let inline _JObject x = (prism' JObject <| function JObject s -> Some s | _ -> None) x
        let inline _JArray  x = (prism' JArray  <| function JArray  s -> Some s | _ -> None) x
        let inline _JBool   x = (prism' JBool   <| function JBool   s -> Some s | _ -> None) x
        let inline _JNumber x = (prism' JNumber <| fun v -> match ofJson v : decimal ParseResult with Ok s->Some s | _ -> None) x
        let inline _JNull   x = prism' (konst JNull) (function JNull -> Some () | _ -> None) x
        /// Like '_jnth', but for 'Object' with Text indices.
        let inline _jkey i =
            let inline dkey i f t = map (fun x -> IReadOnlyDictionary.add i x t) (f (IReadOnlyDictionary.tryGetValue i t |> Option.defaultValue JNull))
            _JObject << dkey i
        let inline _jnth i =
            let inline dnth i f t = map (fun x -> t |> ReadOnlyList.add i x |> Option.defaultValue t) (f (ReadOnlyList.tryNth i t |> Option.defaultValue JNull))
            _JArray << dnth i