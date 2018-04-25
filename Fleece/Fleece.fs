namespace Fleece

[<AutoOpen>]
module Fleece =

    open System
    open System.Globalization    
    open System.Collections.Generic
    open FSharpPlus
    open ReadOnlyCollectionsExtensions

    type Id1<'t>(v:'t) =
        let value = v
        member __.getValue = value

    type Id2<'t>(v:'t) =
        let value = v
        member __.getValue = value

    type Default3 = class end
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

        member private x.ToDecimal() = x.FoldNumeric(id,decimal)
        member private x.ToDouble() = x.FoldNumeric(double,double)
        member private x.ToSingle() = x.FoldNumeric(single,single)
        member private x.ToInt16() = x.FoldNumeric(int16,int16)
        member private x.ToInt32() = x.FoldNumeric(int,int)
        member private x.ToInt64() = x.FoldNumeric(int64,int64)
        member private x.ToUInt16() = x.FoldNumeric(uint16,uint16)
        member private x.ToUInt32() = x.FoldNumeric(uint32,uint32)
        member private x.ToUInt64() = x.FoldNumeric(uint64,uint64)
        member private x.ToByte() = x.FoldNumeric(byte,byte)
        member private x.ToSByte() = x.FoldNumeric(sbyte,sbyte)
            
    
    type private JsonHelpers() =
        static member create (x : decimal) = JValue x :> JToken
        static member create (x : Double) = JValue x :> JToken
        static member create (x : Single) = JValue (float x) :> JToken
        static member create (x : int) = JValue (decimal x) :> JToken
        static member create (x : bool) = JValue x :> JToken
        static member create (x : uint32) = JValue (decimal x) :> JToken
        static member create (x : int64) = JValue (decimal x) :> JToken
        static member create (x : uint64) = JValue (decimal x) :> JToken
        static member create (x : int16) = JValue (decimal x) :> JToken
        static member create (x : uint16) = JValue (decimal x) :> JToken
        static member create (x : byte) = JValue (decimal x) :> JToken
        static member create (x : sbyte) = JValue (decimal x)  :> JToken
        static member create (x : char) = JValue (string x) :> JToken
        static member create (x : Guid) = JValue (string x) :> JToken


    // FSharp.Newtonsoft.Json AST adapter

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o:JToken) =
        match o.Type with
        | JTokenType.Null -> JNull
        | JTokenType.Array -> JArray ( (o :?> JArray).AsReadOnlyList())
        | JTokenType.Object -> JObject (jsonObjectGetValues (o :?> JObject))
        | JTokenType.Integer  -> JNumber o
        | JTokenType.Float -> JNumber o
        | JTokenType.Boolean -> JBool (o.ToObject() :bool)
        | JTokenType.String -> JString (o.ToObject() :string)
        | t -> failwithf "Invalid JTokenType %A" t
    
    let dictAsProps (x: IReadOnlyDictionary<string, JToken>) = 
        x |> Seq.map (fun p -> p.Key,p.Value) |> Array.ofSeq 

    let inline JArray (x: JToken IReadOnlyList) = JArray (x |> Array.ofSeq) :> JToken
    let inline JObject (x: IReadOnlyDictionary<string, JToken>) = JObject (dictAsProps x) :> JToken
    let inline JBool (x: bool) = JValue x :> JToken
    let JNull = JValue.CreateNull() :> JToken
    let inline JString (x: string) = 
        if x = null 
            then JNull
            else JValue x :> JToken
    
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

        member private x.ToDecimal() = x.FoldNumeric(id,decimal)
        member private x.ToDouble() = x.FoldNumeric(double,double)
        member private x.ToSingle() = x.FoldNumeric(single,single)
        member private x.ToInt16() = x.FoldNumeric(int16,int16)
        member private x.ToInt32() = x.FoldNumeric(int,int)
        member private x.ToInt64() = x.FoldNumeric(int64,int64)
        member private x.ToUInt16() = x.FoldNumeric(uint16,uint16)
        member private x.ToUInt32() = x.FoldNumeric(uint32,uint32)
        member private x.ToUInt64() = x.FoldNumeric(uint64,uint64)
        member private x.ToByte() = x.FoldNumeric(byte,byte)
        member private x.ToSByte() = x.FoldNumeric(sbyte,sbyte)
            
    
    type private JsonHelpers() =
        static member create (x : decimal) : JsonValue = JsonValue.Number x
        static member create (x : Double) : JsonValue = JsonValue.Float x
        static member create (x : Single) : JsonValue = JsonValue.Float (float x)
        static member create (x : int) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : bool) : JsonValue = JsonValue.Boolean x
        static member create (x : uint32) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : int64) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : uint64) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : int16) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : uint16) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : byte) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : sbyte) : JsonValue = JsonValue.Number (decimal x)
        static member create (x : char) : JsonValue = JsonValue.String (string x)
        static member create (x : Guid) : JsonValue = JsonValue.String (string x)


    type private ReadOnlyJsonPropertiesDictionary(properties:(string * JsonValue)[]) =                
        
        let properties = properties

        member __.Properties = properties

        with
            interface System.Collections.IEnumerable with
                member __.GetEnumerator() = (properties |> Seq.map (fun (k,v) -> KeyValuePair(k,v))).GetEnumerator() :> System.Collections.IEnumerator

            interface IEnumerable<KeyValuePair<string, JsonValue>> with
                member __.GetEnumerator() = (properties |> Seq.map (fun (k,v) -> KeyValuePair(k,v))).GetEnumerator()

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
        | JsonValue.Null -> JNull
        | JsonValue.Array els -> JArray (els.AsReadOnlyList())
        | JsonValue.Record props -> JObject (jsonObjectGetValues props)
        | JsonValue.Number _ as x -> JNumber x
        | JsonValue.Float _ as x -> JNumber x
        | JsonValue.Boolean x -> JBool x
        | JsonValue.String x -> JString x
    
    let dictAsProps (x: IReadOnlyDictionary<string, JsonValue>) = 
        match x with
        | :? ReadOnlyJsonPropertiesDictionary as x' -> x'.Properties
        | _ -> x |> Seq.map (fun p -> p.Key,p.Value) |> Array.ofSeq

    let inline JArray (x: JsonValue IReadOnlyList) = JsonValue.Array (x |> Array.ofSeq)
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonValue.Record (dictAsProps x)
    let inline JBool (x: bool) = JsonValue.Boolean x
    let JNull : JsonValue = JsonValue.Null
    let inline JString (x: string) = 
        if x = null 
            then JsonValue.Null
            else JsonValue.String x
    
    #endif
    #if SYSTEMJSON
    
    open System.Json

    type JsonObject with
        member x.AsReadOnlyDictionary() =
            (x :> IDictionary<string, JsonValue>).AsReadOnlyDictionary()

        static member GetValues (x: JsonObject) = x.AsReadOnlyDictionary()

    let jsonObjectGetValues (x : JsonObject) = JsonObject.GetValues x


    type private JsonHelpers() =
        static member create (x: decimal) = JsonPrimitive x :> JsonValue
        static member create (x: Double) = JsonPrimitive x :> JsonValue
        static member create (x: Single) = JsonPrimitive x :> JsonValue
        static member create (x: int) = JsonPrimitive x :> JsonValue
        static member create (x: uint32) = JsonPrimitive x :> JsonValue
        static member create (x: int64) = JsonPrimitive x :> JsonValue
        static member create (x: uint64) = JsonPrimitive x :> JsonValue
        static member create (x: int16) = JsonPrimitive x :> JsonValue
        static member create (x: uint16) = JsonPrimitive x :> JsonValue
        static member create (x: byte) = JsonPrimitive x :> JsonValue
        static member create (x: sbyte) = JsonPrimitive x :> JsonValue
        static member create (x: char) = JsonPrimitive x :> JsonValue
        static member create (x: Guid) = JsonPrimitive x :> JsonValue


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
            | JsonType.Number -> JNumber x
            | JsonType.Boolean -> JBool (x.ReadAs<bool>())
            | JsonType.String -> JString (x.ReadAs<string>())
            | JsonType.Default -> JNull
            | _ -> failwithf "Invalid JsonType %A for primitive %A" x.JsonType x
        | _ -> failwithf "Invalid JsonValue %A" o

    let inline JArray (x: JsonValue IReadOnlyList) = JsonArray x :> JsonValue
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonObject x :> JsonValue
    let inline JBool (x: bool) = JsonPrimitive x :> JsonValue
    let JNull : JsonValue = null
    let inline JString (x: string) = 
        if x = null 
            then JNull
            else JsonPrimitive x :> JsonValue

    #endif


    // results:

    let (|Success|Failure|) =
        function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    // Deserializing:

    type 'a ParseResult = Choice<'a, string>

    module Helpers =
        let inline failparse s v = Failure (sprintf "Expected %s, actual %A" s v)
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
            
            static member inline tryReadDecimal = tryRead<decimal> "decimal"  
            static member inline tryReadInt16 = tryRead<int16> "int16"
            static member inline tryReadInt = tryRead<int> "int"
            static member inline tryReadInt64 = tryRead<int64> "int64"
            static member inline tryReadUInt16 = tryRead<uint16> "uint16"
            static member inline tryReadUInt32 = tryRead<uint32> "uint32"
            static member inline tryReadUInt64 = tryRead<uint64> "uint64"
            static member inline tryReadByte = tryRead<byte> "byte"
            static member inline tryReadSByte = tryRead<sbyte> "sbyte"
            static member inline tryReadDouble = tryRead<double> "double"
            static member inline tryReadSingle = tryRead<single> "single"
                
            static member jsonObjectFromJSON =
                fun (o: JToken) ->
                    match o.Type with
                    | JTokenType.Object -> Success ( o :?> JObject )
                    | a -> failparse "JsonObject" a     

        #endif
        #if FSHARPDATA

        type JsonHelpers with
        
            static member tryReadDecimal = function
                | JNumber n -> n.ToDecimal() |> Success
                | a -> failparse "decimal" a   

            static member tryReadInt16 = function
                | JNumber n -> n.ToInt16() |> Success
                | a -> failparse "int16" a
            
            static member tryReadInt = function
                | JNumber n -> n.ToInt32() |> Success
                | a -> failparse "int" a    

            static member tryReadInt64 = function
                | JNumber n -> n.ToInt64() |> Success
                | a -> failparse "int64" a

            static member tryReadUInt16 = function
                | JNumber n -> n.ToUInt16() |> Success
                | a -> failparse "unint16" a

            static member tryReadUInt32 = function
                | JNumber n -> n.ToUInt32() |> Success
                | a -> failparse "unint32" a

            static member tryReadUInt64 = function
                | JNumber n -> n.ToUInt64() |> Success
                | a -> failparse "unint64" a

            static member tryReadByte = function
                | JNumber n -> n.ToByte() |> Success
                | a -> failparse "byte" a

            static member tryReadSByte = function
                | JNumber n -> n.ToSByte() |> Success
                | a -> failparse "sbyte" a

            static member tryReadDouble = function
                | JNumber n -> n.ToDouble() |> Success
                | a -> failparse "double" a

            static member tryReadSingle = function
                | JNumber n -> n.ToSingle() |> Success
                | a -> failparse "single" a      
                
            static member jsonObjectFromJSON =
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
            
            static member inline tryReadDecimal = tryRead<decimal> "decimal"  
            static member inline tryReadInt16 = tryRead<int16> "int16"
            static member inline tryReadInt = tryRead<int> "int"
            static member inline tryReadInt64 = tryRead<int64> "int64"
            static member inline tryReadUInt16 = tryRead<uint16> "uint16"
            static member inline tryReadUInt32 = tryRead<uint32> "uint32"
            static member inline tryReadUInt64 = tryRead<uint64> "uint64"
            static member inline tryReadByte = tryRead<byte> "byte"
            static member inline tryReadSByte = tryRead<sbyte> "sbyte"
            static member inline tryReadDouble = tryRead<double> "double"
            static member inline tryReadSingle = tryRead<single> "single"

            static member inline jsonObjectFromJSON =
                fun (o: JsonValue) ->
                    match box o with
                    | :? JsonObject as x -> Success x
                    | a -> failparse "JsonObject" a

        #endif


        let inline iFromJSON (a: ^a, b: ^b) =
            ((^a or ^b) : (static member FromJSON: ^b*_ -> (JsonValue -> ^b ParseResult)) b, a)

        let inline iToJSON (_: ^a, b: ^b) =
            ((^a or ^b) : (static member ToJSON: ^b -> JsonValue) b)

        let listAsReadOnly (l: _ list) =
            { new IReadOnlyList<_> with
                member __.Count = l.Length
                member __.Item with get index = l.[index]
                member __.GetEnumerator() = (l :> _ seq).GetEnumerator()
                member __.GetEnumerator() = (l :> System.Collections.IEnumerable).GetEnumerator() }

        let dict x = (dict x).AsReadOnlyDictionary()

        let keys (x: IReadOnlyDictionary<_,_>) =
            Seq.map (fun (KeyValue(k,_)) -> k) x

        let values (x: IReadOnlyDictionary<_,_>) =
            Seq.map (fun (KeyValue(_,v)) -> v) x

        let inline notNull a = not (obj.ReferenceEquals(a, null))

    open Helpers

    type FromJSONClass =
        inherit Default1
        
        static member FromJSON (_: bool, _:FromJSONClass) = 
            function
            | JBool b -> Success b
            | a -> failparse "bool" a

        static member FromJSON (_: string, _:FromJSONClass) =
            function
            | JString b -> Success b
            | JNull -> Success null
            | a -> failparse "string" a

        static member FromJSON (_:decimal, _:FromJSONClass) = JsonHelpers.tryReadDecimal        
        static member FromJSON (_:int16, _:FromJSONClass) = JsonHelpers.tryReadInt16
        static member FromJSON (_:int, _:FromJSONClass) = JsonHelpers.tryReadInt        
        static member FromJSON (_:int64, _:FromJSONClass) = JsonHelpers.tryReadInt64                
        static member FromJSON (_:uint16, _:FromJSONClass) = JsonHelpers.tryReadUInt16
        static member FromJSON (_:uint32, _:FromJSONClass) = JsonHelpers.tryReadUInt32
        static member FromJSON (_:uint64, _:FromJSONClass) = JsonHelpers.tryReadUInt64
        static member FromJSON (_:byte, _:FromJSONClass) = JsonHelpers.tryReadByte
        static member FromJSON (_:sbyte, _:FromJSONClass) = JsonHelpers.tryReadSByte
        static member FromJSON (_:double, _:FromJSONClass) = JsonHelpers.tryReadDouble
        static member FromJSON (_:single, _:FromJSONClass) = JsonHelpers.tryReadSingle

        static member FromJSON (_: char, _:FromJSONClass) =
            function
            | JString s -> 
                if s = null
                    then Failure "Expected char, got null"
                    else Success s.[0]
            | a -> failparse "char" a

        static member FromJSON (_: Guid, _:FromJSONClass) =
            function
            | JString s -> 
                if s = null
                    then Failure "Expected Guid, got null"
                    else 
                        match Guid.TryParse s with
                        | false, _ -> Failure ("Invalid Guid " + s)
                        | true, g -> Success g
            | a -> failparse "Guid" a

        static member FromJSON (_: DateTime, _:FromJSONClass) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTime, got null"
                    else match DateTime.TryParseExact(s, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTime %s" s)
            | a -> failparse "DateTime" a

        static member FromJSON (_: DateTimeOffset, _:FromJSONClass) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTimeOffset, got null"
                    else match DateTimeOffset.TryParseExact(s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTimeOffset %s" s)
            | a -> failparse "DateTimeOffset" a

    /// Maps JSON to a type
    let inline fromJSON (x: JsonValue) : 'a ParseResult = iFromJSON (Unchecked.defaultof<FromJSONClass>, Unchecked.defaultof<'a>) x

    /// Parses JSON and maps to a type
    let inline parseJSON (x: string) : 'a ParseResult =
        try
            let json = JsonValue.Parse x
            fromJSON json
        with e -> Failure (e.ToString())

    /// Gets a value from a JSON object
    let inline jget (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> fromJSON value
        | _ -> Failure ("Key '" + key + "' not found in " + JObject(o).ToString())

    /// Tries to get a value from a JSON object.
    /// Returns None if key is not present in the object.
    let inline jgetopt (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> fromJSON value |> map Some
        | _ -> Success None

    type FromJSONClass with
        static member inline FromJSON (_: Choice<'a, 'b>, _:FromJSONClass) : JsonValue -> ParseResult<Choice<'a, 'b>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> fromJSON |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> fromJSON |> map Choice2Of2
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type FromJSONClass with
        static member inline FromJSON (_: Choice<'a, 'b, 'c>, _:FromJSONClass) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> fromJSON |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> fromJSON |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> fromJSON |> map Choice3Of3
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type FromJSONClass with
        static member inline FromJSON (_: 'a option, _:FromJSONClass) : JsonValue -> ParseResult<'a option> =
            function
            | JNull _ -> Success None
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map Some a

    type FromJSONClass with
        static member inline FromJSON (_: 'a Nullable, _:FromJSONClass) : JsonValue -> ParseResult<'a Nullable> =
            function
            | JNull _ -> Success (Nullable())
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map (fun x -> Nullable x) a

    type FromJSONClass with
        static member inline FromJSON (_: 'a array, _:FromJSONClass) : JsonValue -> ParseResult<'a array> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a 
                map Seq.toArray xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: list<'a>, _:FromJSONClass) : JsonValue -> ParseResult<list<'a>> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map Seq.toList xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: 'a Set, _:FromJSONClass) : JsonValue -> ParseResult<'a Set> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map set xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: Map<string, 'a>, _:FromJSONClass) : JsonValue -> ParseResult<Map<string, 'a>> =
            function
            | JObject o ->
                let xx : 'a seq ParseResult = traverse fromJSON (values o)
                map (fun values -> Seq.zip (keys o) values |> Map.ofSeq) xx
            | a -> failparse "Map" a

    type FromJSONClass with
        static member inline FromJSON (_: Dictionary<string, 'a>, _:FromJSONClass) : JsonValue -> ParseResult<Dictionary<string, 'a>> =
            function
            | JObject o ->
                let xx : 'a seq ParseResult = traverse fromJSON (values o)
                xx |> map (fun values ->
                        let kv = Seq.zip (keys o) values
                        let d = Dictionary()
                        for k,v in kv do d.[k] <- v
                        d)
            | a -> failparse "Dictionary" a

        static member inline FromJSON (_: 'a ResizeArray, _:FromJSONClass) : JsonValue -> ParseResult<'a ResizeArray> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map (fun x -> ResizeArray<_>(x: 'a seq)) xx
            | a -> failparse "ResizeArray" a

        static member inline FromJSON (_: 'a Id1, _:FromJSONClass) : JsonValue -> ParseResult<Id1<'a>> = fun _ -> Success (Id1<'a>(Unchecked.defaultof<'a>))
        static member inline FromJSON (_: 'a Id2, _:FromJSONClass) : JsonValue -> ParseResult<Id2<'a>> = fun _ -> Success (Id2<'a>(Unchecked.defaultof<'a>))

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b> =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + x.ToString())
                else
                    tuple2 <!> (fromJSON a.[0]) <*> (fromJSON a.[1])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c> =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + x.ToString())
                else
                    tuple3 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + x.ToString())
                else
                    tuple4 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + x.ToString())
                else
                    tuple5 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + x.ToString())
                else
                    tuple6 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, _:FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + x.ToString())
                else
                    tuple7 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5]) <*> (fromJSON a.[6])
            | a -> Failure (sprintf "Expected array, found %A" a)

    // Default, for external classes.
    type FromJSONClass with 
        static member inline FromJSON (r: 'R, _:Default3) = (^R : (static member FromJSON: ^R   -> (JsonValue -> ^R ParseResult)) r ) : JsonValue ->  ^R ParseResult
        static member inline FromJSON (_: 'R, _:Default2) = fun js -> (^R : (static member FromJSON: JsonValue -> ^R ParseResult) js) : ^R ParseResult

        static member FromJSON (_:JsonObject, _:Default1) = JsonHelpers.jsonObjectFromJSON
        
    // Serializing:

    type ToJSONClass = ToJSONClass with
        static member ToJSON (x: bool) = JBool x
        static member ToJSON (x: string) = JString x
        static member ToJSON (x: DateTime) = JString (x.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")) // JsonPrimitive is incorrect for DateTime
        static member ToJSON (x: DateTimeOffset) = JString (x.ToString("yyyy-MM-ddTHH:mm:ss.fffK")) // JsonPrimitive is incorrect for DateTimeOffset
        static member ToJSON (x: decimal) = JsonHelpers.create x
        static member ToJSON (x: Double) = JsonHelpers.create x
        static member ToJSON (x: Single) = JsonHelpers.create x
        static member ToJSON (x: int) = JsonHelpers.create x
        static member ToJSON (x: uint32) = JsonHelpers.create x
        static member ToJSON (x: int64) = JsonHelpers.create x
        static member ToJSON (x: uint64) = JsonHelpers.create x
        static member ToJSON (x: int16) = JsonHelpers.create x
        static member ToJSON (x: uint16) = JsonHelpers.create x
        static member ToJSON (x: byte) = JsonHelpers.create x
        static member ToJSON (x: sbyte) = JsonHelpers.create x
        static member ToJSON (x: char) = JsonHelpers.create x
        static member ToJSON (x: Guid) = JsonHelpers.create x


    /// Maps a value to JSON
    let inline toJSON (x: 'a) : JsonValue = iToJSON (ToJSONClass, x)

    /// Creates a new JSON object for serialization
    let jobj x = JObject (x |> Seq.filter (fun (k,_) -> notNull k) |> dict)

    /// Creates a new JSON key,value pair for a JSON object
    let inline jpair (key: string) value = key, toJSON value

    type ToJSONClass with
        static member inline ToJSON (x: Choice<'a, 'b>) =
            match x with
            | Choice1Of2 a -> jobj [ jpair "Choice1Of2" a ]
            | Choice2Of2 a -> jobj [ jpair "Choice2Of2" a ]

    type ToJSONClass with
        static member inline ToJSON (x: Choice<'a, 'b, 'c>) =
            match x with
            | Choice1Of3 a -> jobj [ jpair "Choice1Of3" a ]
            | Choice2Of3 a -> jobj [ jpair "Choice2Of3" a ]
            | Choice3Of3 a -> jobj [ jpair "Choice3Of3" a ]

    type ToJSONClass with
        static member inline ToJSON (x: 'a option) =
            match x with
            | None -> JNull
            | Some a -> toJSON a

    type ToJSONClass with
        static member inline ToJSON (x: 'a Nullable) =
            if x.HasValue 
                then toJSON x.Value
                else JNull

    type ToJSONClass with
        static member inline ToJSON (x: list<'a>) =
            JArray (listAsReadOnly (List.map toJSON x))

    type ToJSONClass with
        static member inline ToJSON (x: 'a Set) =
            JArray ((Seq.map toJSON x).ToReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON (x: 'a array) =
            JArray ((Array.map toJSON x).AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON (x: Map<string, 'a>) =
            let v = x |> Seq.filter (fun (KeyValue(k, _)) -> notNull k) |> Seq.map (fun (KeyValue(k,v)) -> k, toJSON v) |> dict
            JObject v

    type ToJSONClass with
        static member inline ToJSON (x: Dictionary<string, 'a>) =
            let v = x |> Seq.filter (fun (KeyValue(k, _)) -> notNull k) |> Seq.map (fun (KeyValue(k,v)) -> k, toJSON v) |> dict
            JObject v

        static member inline ToJSON (x: 'a ResizeArray) =
            JArray ((Seq.map toJSON x).ToReadOnlyList())

        static member inline ToJSON ((a, b)) =
            JArray ([|toJSON a; toJSON b|].AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON ((a, b, c)) =
            JArray ([|toJSON a; toJSON b; toJSON c|].AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON ((a, b, c, d)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d|].AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON ((a, b, c, d, e)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e|].AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON ((a, b, c, d, e, f)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f|].AsReadOnlyList())

    type ToJSONClass with
        static member inline ToJSON ((a, b, c, d, e, f, g)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f; toJSON g|].AsReadOnlyList())

    module Operators =
        /// Creates a new JSON key,value pair for a JSON object
        let inline (.=) key value = jpair key value

        /// Gets a value from a JSON object
        let inline (.@) o key = jget o key

        /// Tries to get a value from a JSON object.
        /// Returns None if key is not present in the object.
        let inline (.@?) o key = jgetopt o key