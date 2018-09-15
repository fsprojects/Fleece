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
    // unify JsonValue.Number and JsonValue.Float
    type JValue with
        
        member private x.FoldNumeric (e:decimal -> 'a, f:float -> 'a) : 'a =
            match x.Type with
            | JTokenType.Integer -> e (x.ToObject())
            | JTokenType.Float -> f (x.ToObject())
            | j -> failwith (sprintf "Expected numeric but was %A" j)

        member private x.ToDecimal () = x.FoldNumeric (id    , decimal)
        member private x.ToDouble ()  = x.FoldNumeric (double, double)
        member private x.ToSingle ()  = x.FoldNumeric (single, single)
        member private x.ToInt16 ()   = x.FoldNumeric (int16 , int16)
        member private x.ToInt32 ()   = x.FoldNumeric (int   , int)
        member private x.ToInt64 ()   = x.FoldNumeric (int64 , int64)
        member private x.ToUInt16 ()  = x.FoldNumeric (uint16, uint16)
        member private x.ToUInt32 ()  = x.FoldNumeric (uint32, uint32)
        member private x.ToUInt64 ()  = x.FoldNumeric (uint64, uint64)
        member private x.ToByte ()    = x.FoldNumeric (byte  , byte)
        member private x.ToSByte ()   = x.FoldNumeric (sbyte , sbyte)
            
    
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

    // unify JsonValue.Number and JsonValue.Float
    type JsonValue with
        
        member private x.FoldNumeric (e:decimal -> 'a, f:float -> 'a) : 'a =
            match x with
            | JsonValue.Number n -> e n
            | JsonValue.Float n -> f n 
            | j -> failwith (sprintf "Expected numeric but was %A" j)

        member private x.ToDecimal () = x.FoldNumeric (id    , decimal)
        member private x.ToDouble ()  = x.FoldNumeric (double, double)
        member private x.ToSingle ()  = x.FoldNumeric (single, single)
        member private x.ToInt16 ()   = x.FoldNumeric (int16 , int16)
        member private x.ToInt32 ()   = x.FoldNumeric (int   , int)
        member private x.ToInt64 ()   = x.FoldNumeric (int64 , int64)
        member private x.ToUInt16 ()  = x.FoldNumeric (uint16, uint16)
        member private x.ToUInt32 ()  = x.FoldNumeric (uint32, uint32)
        member private x.ToUInt64 ()  = x.FoldNumeric (uint64, uint64)
        member private x.ToByte ()    = x.FoldNumeric (byte  , byte)
        member private x.ToSByte ()   = x.FoldNumeric (sbyte , sbyte)
            
    
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

    type Decimal with static member OfJson x = Helpers.tryRead<decimal> "decimal" x
    type Int16   with static member OfJson x = Helpers.tryRead<int16>   "int16"   x
    type Int32   with static member OfJson x = Helpers.tryRead<int>     "int"     x
    type Int64   with static member OfJson x = Helpers.tryRead<int64>   "int64"   x
    type UInt16  with static member OfJson x = Helpers.tryRead<uint16>  "uint16"  x
    type UInt32  with static member OfJson x = Helpers.tryRead<uint32>  "uint32"  x
    type UInt64  with static member OfJson x = Helpers.tryRead<uint64>  "uint64"  x
    type Byte    with static member OfJson x = Helpers.tryRead<byte>    "byte"    x
    type SByte   with static member OfJson x = Helpers.tryRead<sbyte>   "sbyte"   x
    type Double  with static member OfJson x = Helpers.tryRead<double>  "double"  x
    type Single  with static member OfJson x = Helpers.tryRead<single>  "single"  x
 

        #endif
        #if FSHARPDATA

        type JsonHelpers with
            static member jsonObjectOfJson =
                fun (o: JsonValue) ->
                    match o with
                    | JObject x -> Success (dictAsProps x)
                    | a -> failparse "JsonObject" a

    type Decimal with static member OfJson x = match x with JNumber n -> n.ToDecimal () |> Success | a -> Helpers.failparse "decimal" a
    type Int16   with static member OfJson x = match x with JNumber n -> n.ToInt16 ()   |> Success | a -> Helpers.failparse "int16"   a
    type Int32   with static member OfJson x = match x with JNumber n -> n.ToInt32 ()   |> Success | a -> Helpers.failparse "int"     a
    type Int64   with static member OfJson x = match x with JNumber n -> n.ToInt64 ()   |> Success | a -> Helpers.failparse "int64"   a
    type UInt16  with static member OfJson x = match x with JNumber n -> n.ToUInt16 ()  |> Success | a -> Helpers.failparse "unint16" a
    type UInt32  with static member OfJson x = match x with JNumber n -> n.ToUInt32 ()  |> Success | a -> Helpers.failparse "unint32" a
    type UInt64  with static member OfJson x = match x with JNumber n -> n.ToUInt64 ()  |> Success | a -> Helpers.failparse "unint64" a
    type Byte    with static member OfJson x = match x with JNumber n -> n.ToByte ()    |> Success | a -> Helpers.failparse "byte"    a
    type SByte   with static member OfJson x = match x with JNumber n -> n.ToSByte ()   |> Success | a -> Helpers.failparse "sbyte"   a
    type Double  with static member OfJson x = match x with JNumber n -> n.ToDouble ()  |> Success | a -> Helpers.failparse "double"  a
    type Single  with static member OfJson x = match x with JNumber n -> n.ToSingle ()  |> Success | a -> Helpers.failparse "single"  a


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

    type Decimal with static member OfJson x = Helpers.tryRead<decimal> "decimal" x
    type Int16   with static member OfJson x = Helpers.tryRead<int16>   "int16"   x
    type Int32   with static member OfJson x = Helpers.tryRead<int>     "int"     x
    type Int64   with static member OfJson x = Helpers.tryRead<int64>   "int64"   x
    type UInt16  with static member OfJson x = Helpers.tryRead<uint16>  "uint16"  x
    type UInt32  with static member OfJson x = Helpers.tryRead<uint32>  "uint32"  x
    type UInt64  with static member OfJson x = Helpers.tryRead<uint64>  "uint64"  x
    type Byte    with static member OfJson x = Helpers.tryRead<byte>    "byte"    x
    type SByte   with static member OfJson x = Helpers.tryRead<sbyte>   "sbyte"   x
    type Double  with static member OfJson x = Helpers.tryRead<double>  "double"  x
    type Single  with static member OfJson x = Helpers.tryRead<single>  "single"  x


        #endif


    open Helpers

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

    type OfJsonClass =
        inherit Default1
        
        static member OfJson (_: decimal, _: OfJsonClass) = Decimal.OfJson
        static member OfJson (_: int16  , _: OfJsonClass) = Int16  .OfJson
        static member OfJson (_: int    , _: OfJsonClass) = Int32  .OfJson
        static member OfJson (_: int64  , _: OfJsonClass) = Int64  .OfJson
        static member OfJson (_: uint16 , _: OfJsonClass) = UInt16 .OfJson
        static member OfJson (_: uint32 , _: OfJsonClass) = UInt32 .OfJson
        static member OfJson (_: uint64 , _: OfJsonClass) = UInt64 .OfJson
        static member OfJson (_: byte   , _: OfJsonClass) = Byte   .OfJson
        static member OfJson (_: sbyte  , _: OfJsonClass) = SByte  .OfJson
        static member OfJson (_: double , _: OfJsonClass) = Double .OfJson
        static member OfJson (_: single , _: OfJsonClass) = Single .OfJson

        static member OfJson (_: bool          , _: OfJsonClass) = Boolean       .OfJson
        static member OfJson (_: string        , _: OfJsonClass) = String        .OfJson
        static member OfJson (_: char          , _: OfJsonClass) = Char          .OfJson
        static member OfJson (_: Guid          , _: OfJsonClass) = Guid          .OfJson
        static member OfJson (_: DateTime      , _: OfJsonClass) = DateTime      .OfJson
        static member OfJson (_: DateTimeOffset, _: OfJsonClass) = DateTimeOffset.OfJson

    type OfJsonClass with
        static member inline Invoke (x: JsonValue) : 't ParseResult =
            let inline iOfJson (a: ^a, b: ^b) = ((^a or ^b) : (static member OfJson: ^b * _ -> (JsonValue -> ^b ParseResult)) b, a)
            iOfJson (Unchecked.defaultof<OfJsonClass>, Unchecked.defaultof<'t>) x

    /// Maps Json to a type
    let inline ofJson (x: JsonValue) : 't ParseResult = OfJsonClass.Invoke x

    [<Obsolete("Use 'ofJson'")>]
    let inline fromJSON (x: JsonValue) : 't ParseResult = OfJsonClass.Invoke x

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

    type OfJsonClass with
        static member inline OfJson (_: Choice<'a, 'b>, _:OfJsonClass) : JsonValue -> ParseResult<Choice<'a, 'b>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> ofJson |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> ofJson |> map Choice2Of2
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type OfJsonClass with
        static member inline OfJson (_: Choice<'a, 'b, 'c>, _:OfJsonClass) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> ofJson |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> ofJson |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> ofJson |> map Choice3Of3
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type OfJsonClass with
        static member inline OfJson (_: 'a option, _:OfJsonClass) : JsonValue -> ParseResult<'a option> =
            function
            | JNull _ -> Success None
            | x -> 
                let a: 'a ParseResult = ofJson x
                map Some a

    type OfJsonClass with
        static member inline OfJson (_: 'a Nullable, _:OfJsonClass) : JsonValue -> ParseResult<'a Nullable> =
            function
            | JNull _ -> Success (Nullable())
            | x -> 
                let a: 'a ParseResult = ofJson x
                map (fun x -> Nullable x) a

    type OfJsonClass with
        static member inline OfJson (_: 'a array, _:OfJsonClass) : JsonValue -> ParseResult<'a array> =
            function
            | JArray a -> traverse ofJson a |> map Seq.toArray
            | a -> failparse "array" a

    type OfJsonClass with
        static member inline OfJson (_: list<'a>, _:OfJsonClass) : JsonValue -> ParseResult<list<'a>> =
            function
            | JArray a -> traverse ofJson a |> map Seq.toList
            | a -> failparse "array" a

    type OfJsonClass with
        static member inline OfJson (_: 'a Set, _:OfJsonClass) : JsonValue -> ParseResult<'a Set> =
            function
            | JArray a -> traverse ofJson a |> map set
            | a -> failparse "array" a

    type OfJsonClass with
        static member inline OfJson (_: Map<string, 'a>, _:OfJsonClass) : JsonValue -> ParseResult<Map<string, 'a>> =
            function
            | JObject o -> traverse ofJson (values o) |> map (fun values -> Seq.zip (keys o) values |> Map.ofSeq)
            | a -> failparse "Map" a

    type OfJsonClass with
        static member inline OfJson (_: Dictionary<string, 'a>, _:OfJsonClass) : JsonValue -> ParseResult<Dictionary<string, 'a>> =
            function
            | JObject o -> traverse ofJson (values o) |> map (fun values -> Seq.zip (keys o) values |> ofSeq)
            | a -> failparse "Dictionary" a

        static member inline OfJson (_: 'a ResizeArray, _:OfJsonClass) : JsonValue -> ParseResult<'a ResizeArray> =
            function
            | JArray a -> traverse ofJson a |> map (fun x -> ResizeArray<_>(x: 'a seq))
            | a -> failparse "ResizeArray" a

        static member inline OfJson (_: 'a Id1, _:OfJsonClass) : JsonValue -> ParseResult<Id1<'a>> = fun _ -> Success (Id1<'a>(Unchecked.defaultof<'a>))
        static member inline OfJson (_: 'a Id2, _:OfJsonClass) : JsonValue -> ParseResult<Id2<'a>> = fun _ -> Success (Id2<'a>(Unchecked.defaultof<'a>))

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b> =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + string x)
                else
                    tuple2 <!> ofJson a.[0] <*> ofJson a.[1]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b * 'c, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b * 'c> =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + string x)
                else
                    tuple3 <!> ofJson a.[0] <*> ofJson a.[1] <*> ofJson a.[2]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b * 'c * 'd, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + string x)
                else
                    tuple4 <!> ofJson a.[0] <*> ofJson a.[1] <*> ofJson a.[2] <*> ofJson a.[3]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + string x)
                else
                    tuple5 <!> ofJson a.[0] <*> ofJson a.[1] <*> ofJson a.[2] <*> ofJson a.[3] <*> ofJson a.[4]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + string x)
                else
                    tuple6 <!> ofJson a.[0] <*> ofJson a.[1] <*> ofJson a.[2] <*> ofJson a.[3] <*> ofJson a.[4] <*> ofJson a.[5]
            | a -> Failure (sprintf "Expected array, found %A" a)

    type OfJsonClass with
        static member inline OfJson (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, _:OfJsonClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + string x)
                else
                    tuple7 <!> ofJson a.[0] <*> ofJson a.[1] <*> ofJson a.[2] <*> ofJson a.[3] <*> ofJson a.[4] <*> ofJson a.[5] <*> ofJson a.[6]
            | a -> Failure (sprintf "Expected array, found %A" a)



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


    // Default, for external classes.
    type OfJsonClass with 
        static member inline OfJson (_: 'R, _:Default4) =
            let codec = (^R : (static member JsonObjCodec: Codec<IReadOnlyDictionary<string,JsonValue>,'R>) ())
            function
            | JObject o -> decode (fst codec) o : ^R ParseResult
            | a         -> failparse "Map" a

        static member inline OfJson (r: 'R, _:Default3) = (^R : (static member FromJSON: ^R  -> (JsonValue -> ^R ParseResult)) r) : JsonValue ->  ^R ParseResult
        static member inline OfJson (_: 'R, _:Default2) = fun js -> (^R : (static member OfJson: JsonValue -> ^R ParseResult) js) : ^R ParseResult

        static member OfJson (_:JsonObject, _:Default1) = JsonHelpers.jsonObjectOfJson
        
    // Serializing:

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


    type ToJsonClass =
        inherit Default1
        static member ToJson (x: bool          , _:ToJsonClass) = Boolean       .ToJson x
        static member ToJson (x: string        , _:ToJsonClass) = String        .ToJson x
        static member ToJson (x: DateTime      , _:ToJsonClass) = DateTime      .ToJson x
        static member ToJson (x: DateTimeOffset, _:ToJsonClass) = DateTimeOffset.ToJson x
        static member ToJson (x: decimal       , _:ToJsonClass) = Decimal       .ToJson x
        static member ToJson (x: Double        , _:ToJsonClass) = Double        .ToJson x
        static member ToJson (x: Single        , _:ToJsonClass) = Single        .ToJson x
        static member ToJson (x: int           , _:ToJsonClass) = Int32         .ToJson x
        static member ToJson (x: uint32        , _:ToJsonClass) = UInt32        .ToJson x
        static member ToJson (x: int64         , _:ToJsonClass) = Int64         .ToJson x
        static member ToJson (x: uint64        , _:ToJsonClass) = UInt64        .ToJson x
        static member ToJson (x: int16         , _:ToJsonClass) = Int16         .ToJson x
        static member ToJson (x: uint16        , _:ToJsonClass) = UInt16        .ToJson x
        static member ToJson (x: byte          , _:ToJsonClass) = Byte          .ToJson x
        static member ToJson (x: sbyte         , _:ToJsonClass) = SByte         .ToJson x
        static member ToJson (x: char          , _:ToJsonClass) = Char          .ToJson x
        static member ToJson (x: Guid          , _:ToJsonClass) = Guid          .ToJson x


    type ToJsonClass with
        static member inline Invoke (x: 't) : JsonValue =
            let inline iToJson (a: ^a, b: ^b) = ((^a or ^b) : (static member ToJson: ^b * _ -> JsonValue) b, a)
            iToJson (Unchecked.defaultof<ToJsonClass>, x)

    /// Maps a value to Json
    let inline toJson (x: 't) : JsonValue = ToJsonClass.Invoke x

    [<Obsolete("Use 'toJson'")>]
    let inline toJSON (x: 't) : JsonValue = ToJsonClass.Invoke x

    /// Creates a new Json object for serialization
    let jobj x = JObject (x |> Seq.filter (fun (k,_) -> not (isNull k)) |> dict)

    /// Creates a new Json key,value pair for a Json object
    let inline jpair (key: string) value = key, toJson value
    
    /// Creates a new Json key,value pair for a Json object if the value option is present
    let inline jpairopt (key: string) value = match value with Some value -> (key, toJson value) | _ -> (null, JNull)

    type ToJsonClass with
        static member inline ToJson (x: Choice<'a, 'b>, _:ToJsonClass) =
            match x with
            | Choice1Of2 a -> jobj [ jpair "Choice1Of2" a ]
            | Choice2Of2 a -> jobj [ jpair "Choice2Of2" a ]

    type ToJsonClass with
        static member inline ToJson (x: Choice<'a, 'b, 'c>, _:ToJsonClass) =
            match x with
            | Choice1Of3 a -> jobj [ jpair "Choice1Of3" a ]
            | Choice2Of3 a -> jobj [ jpair "Choice2Of3" a ]
            | Choice3Of3 a -> jobj [ jpair "Choice3Of3" a ]

    type ToJsonClass with
        static member inline ToJson (x: 'a option, _:ToJsonClass) =
            match x with
            | None -> JNull
            | Some a -> toJson a

    type ToJsonClass with
        static member inline ToJson (x: 'a Nullable, _:ToJsonClass) =
            if x.HasValue 
                then toJson x.Value
                else JNull

    type ToJsonClass with
        static member inline ToJson (x: list<'a>, _:ToJsonClass) =
            JArray (listAsReadOnly (List.map toJson x))

    type ToJsonClass with
        static member inline ToJson (x: 'a Set, _:ToJsonClass) =
            JArray ((Seq.map toJson x).ToReadOnlyList())

    type ToJsonClass with
        static member inline ToJson (x: 'a array, _:ToJsonClass) =
            JArray ((Array.map toJson x).AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson (x: Map<string, 'a>, _:ToJsonClass) =
            x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k,v)) -> k, toJson v) |> dict |> JObject

    type ToJsonClass with
        static member inline ToJson (x: Dictionary<string, 'a>, _:ToJsonClass) =
            x |> Seq.filter (fun (KeyValue(k, _)) -> not (isNull k)) |> Seq.map (fun (KeyValue(k,v)) -> k, toJson v) |> dict |> JObject

        static member inline ToJson (x: 'a ResizeArray, _:ToJsonClass) =
            JArray ((Seq.map toJson x).ToReadOnlyList())

        static member inline ToJson ((a, b), _:ToJsonClass) =
            JArray ([|toJson a; toJson b|].AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson ((a, b, c), _:ToJsonClass) =
            JArray ([|toJson a; toJson b; toJson c|].AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson ((a, b, c, d), _:ToJsonClass) =
            JArray ([|toJson a; toJson b; toJson c; toJson d|].AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson ((a, b, c, d, e), _:ToJsonClass) =
            JArray ([|toJson a; toJson b; toJson c; toJson d; toJson e|].AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson ((a, b, c, d, e, f), _:ToJsonClass) =
            JArray ([|toJson a; toJson b; toJson c; toJson d; toJson e; toJson f|].AsReadOnlyList())

    type ToJsonClass with
        static member inline ToJson ((a, b, c, d, e, f, g), _:ToJsonClass) =
            JArray ([|toJson a; toJson b; toJson c; toJson d; toJson e; toJson f; toJson g|].AsReadOnlyList())

    // Default, for external classes.
    type ToJsonClass with
        static member inline ToJson (t: 'T, _:Default4) = 
            let codec = (^T : (static member JsonObjCodec: Codec<IReadOnlyDictionary<string,JsonValue>,'T>) ())
            JObject (encode (snd codec) t)

        static member inline ToJson (t: 'T, _:Default3) = (^T : (static member ToJSON: ^T -> JsonValue) t)
        static member inline ToJson (t: 'T, _:Default2) = (^T : (static member ToJson: ^T -> JsonValue) t)

   
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
        let inline _String x= (prism JString <| fun v -> match v with JString s -> Ok s| _ -> Error v) x
        let inline _Object x= (prism JObject <| fun v -> match v with JObject s -> Ok s| _ -> Error v) x
        let inline _Array x= (prism JArray <| fun v -> match v with JArray s -> Ok s| _ -> Error v) x
        let inline _Bool x= (prism JBool <| fun v -> match v with JBool s -> Ok s| _ -> Error v) x
        let inline _Number x= (prism JNumber <| fun v -> match JsonHelpers.tryReadDecimal v with Success s -> Ok s| _ -> Error v) x
        let inline _Null x = prism (konst null) (fun v -> match v with JNull -> Ok ()| _ -> Error null) x
        /// Like '_nth', but for 'Object' with Text indices.
        let inline _key i =
            let inline dkey i f t = map (fun x -> IReadOnlyDictionary.add i x t) (f (IReadOnlyDictionary.tryGetValue i t |> Option.defaultValue JNull))
            _Object << dkey i
        let inline _nth i =
            let inline dnth i f t = map (fun x -> t |> ReadOnlyList.add i x |> Option.defaultValue t) (f (ReadOnlyList.tryNth i t |> Option.defaultValue JNull))
            _Array << dnth i
