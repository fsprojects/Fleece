namespace Fleece

[<AutoOpen>]
module Fleece =

    open System
    open System.Globalization    
    open System.Collections.Generic
    open FSharpPlus
    open ReadOnlyCollectionsExtensions

    type Id<'t>(v:'t) =
        let value = v
        member this.getValue = value

    type JsonValue =
        | JsonObject of Map<string,JsonValue>
        | JsonArray of JsonValue list
        | JsonString of string
        | JsonNumber of float
        | JsonNull
        | JsonBool of bool


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



        let inline iFromJSON (a: ^a, b: ^b) =
            ((^a or ^b) : (static member FromJSON: ^b*_ -> (JsonValue -> ^b ParseResult)) b, a)

        let inline iToJSON (a: ^a, b: ^b) =
            ((^a or ^b) : (static member ToJSON: ^b -> JsonValue) b)

        let listAsReadOnly (l: _ list) =
            { new IReadOnlyList<_> with
                member x.Count = l.Length
                member x.Item with get index = l.[index]
                member x.GetEnumerator() = (l :> _ seq).GetEnumerator()
                member x.GetEnumerator() = (l :> System.Collections.IEnumerable).GetEnumerator() }

        let dict x = (dict x).AsReadOnlyDictionary()

        let keys (x: IReadOnlyDictionary<_,_>) =
            Seq.map (fun (KeyValue(k,_)) -> k) x

        let values (x: IReadOnlyDictionary<_,_>) =
            Seq.map (fun (KeyValue(_,v)) -> v) x

        let inline notNull a = not (obj.ReferenceEquals(a, null))

    open Helpers

    type FromJSONClass = FromJSONClass with
        
        static member FromJSON (_: bool, FromJSONClass) = 
            function
            | JBool b -> Success b
            | a -> failparse "bool" a

        static member FromJSON (_: string, FromJSONClass) =
            function
            | JString b -> Success b
            | JNull -> Success null
            | a -> failparse "string" a

        static member FromJSON (_: JsonObject, FromJSONClass) = JsonHelpers.jsonObjectFromJSON
        static member FromJSON (_:decimal, FromJSONClass) = JsonHelpers.tryReadDecimal        
        static member FromJSON (_:int16, FromJSONClass) = JsonHelpers.tryReadInt16
        static member FromJSON (_:int, FromJSONClass) = JsonHelpers.tryReadInt        
        static member FromJSON (_:int64, FromJSONClass) = JsonHelpers.tryReadInt64                
        static member FromJSON (_:uint16, FromJSONClass) = JsonHelpers.tryReadUInt16
        static member FromJSON (_:uint32, FromJSONClass) = JsonHelpers.tryReadUInt32
        static member FromJSON (_:uint64, FromJSONClass) = JsonHelpers.tryReadUInt64
        static member FromJSON (_:byte, FromJSONClass) = JsonHelpers.tryReadByte
        static member FromJSON (_:sbyte, FromJSONClass) = JsonHelpers.tryReadSByte
        static member FromJSON (_:double, FromJSONClass) = JsonHelpers.tryReadDouble
        static member FromJSON (_:single, FromJSONClass) = JsonHelpers.tryReadSingle

        static member FromJSON (_: char, FromJSONClass) =
            function
            | JString s -> 
                if s = null
                    then Failure "Expected char, got null"
                    else Success s.[0]
            | a -> failparse "char" a

        static member FromJSON (_: Guid, FromJSONClass) =
            function
            | JString s -> 
                if s = null
                    then Failure "Expected Guid, got null"
                    else 
                        match Guid.TryParse s with
                        | false, _ -> Failure ("Invalid Guid " + s)
                        | true, g -> Success g
            | a -> failparse "Guid" a

        static member FromJSON (_: DateTime, FromJSONClass) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTime, got null"
                    else match DateTime.TryParseExact(s, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ" |], null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTime %s" s)
            | a -> failparse "DateTime" a

        static member FromJSON (_: DateTimeOffset, FromJSONClass) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTimeOffset, got null"
                    else match DateTimeOffset.TryParseExact(s, [| "yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK" |], null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTimeOffset %s" s)
            | a -> failparse "DateTimeOffset" a

    /// Maps JSON to a type
    let inline fromJSON (x: JsonValue) : 'a ParseResult = iFromJSON (FromJSONClass, Unchecked.defaultof<'a>) x

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
        static member inline FromJSON (_: Choice<'a, 'b>, FromJSONClass) : JsonValue -> ParseResult<Choice<'a, 'b>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> fromJSON |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> fromJSON |> map Choice2Of2
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type FromJSONClass with
        static member inline FromJSON (_: Choice<'a, 'b, 'c>, FromJSONClass) : JsonValue -> ParseResult<Choice<'a, 'b, 'c>> =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> fromJSON |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> fromJSON |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> fromJSON |> map Choice3Of3
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

    type FromJSONClass with
        static member inline FromJSON (_: 'a option, FromJSONClass) : JsonValue -> ParseResult<'a option> =
            function
            | JNull a -> Success None
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map Some a

    type FromJSONClass with
        static member inline FromJSON (_: 'a Nullable, FromJSONClass) : JsonValue -> ParseResult<'a Nullable> =
            function
            | JNull a -> Success (Nullable())
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map (fun x -> Nullable x) a

    type FromJSONClass with
        static member inline FromJSON (_: 'a array, FromJSONClass) : JsonValue -> ParseResult<'a array> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a 
                map Seq.toArray xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: list<'a>, FromJSONClass) : JsonValue -> ParseResult<list<'a>> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map Seq.toList xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: 'a Set, FromJSONClass) : JsonValue -> ParseResult<'a Set> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map set xx
            | a -> failparse "array" a

    type FromJSONClass with
        static member inline FromJSON (_: Map<string, 'a>, FromJSONClass) : JsonValue -> ParseResult<Map<string, 'a>> =
            function
            | JObject o as jobj ->
                let xx : 'a seq ParseResult = traverse fromJSON (values o)
                map (fun values -> Seq.zip (keys o) values |> Map.ofSeq) xx
            | a -> failparse "Map" a

    type FromJSONClass with
        static member inline FromJSON (_: Dictionary<string, 'a>, FromJSONClass) : JsonValue -> ParseResult<Dictionary<string, 'a>> =
            function
            | JObject o as jobj ->
                let xx : 'a seq ParseResult = traverse fromJSON (values o)
                xx |> map (fun values ->
                        let kv = Seq.zip (keys o) values
                        let d = Dictionary()
                        for k,v in kv do d.[k] <- v
                        d)
            | a -> failparse "Dictionary" a

        static member inline FromJSON (_: 'a ResizeArray, FromJSONClass) : JsonValue -> ParseResult<'a ResizeArray> =
            function
            | JArray a -> 
                let xx : 'a seq ParseResult = traverse fromJSON a
                map (fun x -> ResizeArray<_>(x: 'a seq)) xx
            | a -> failparse "ResizeArray" a

        static member inline FromJSON (_: 'a Id, FromJSONClass) : JsonValue -> ParseResult<Id<'a>> = fun _ -> Success (Id<'a>(Unchecked.defaultof<'a>))

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b, FromJSONClass) : JsonValue -> ParseResult<'a * 'b> =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + x.ToString())
                else
                    tuple2 <!> (fromJSON a.[0]) <*> (fromJSON a.[1])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c, FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c> =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + x.ToString())
                else
                    tuple3 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd, FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd> =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + x.ToString())
                else
                    tuple4 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e, FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e> =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + x.ToString())
                else
                    tuple5 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f, FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f> =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + x.ToString())
                else
                    tuple6 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5])
            | a -> Failure (sprintf "Expected array, found %A" a)

    type FromJSONClass with
        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g, FromJSONClass) : JsonValue -> ParseResult<'a * 'b * 'c * 'd * 'e * 'f * 'g> =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + x.ToString())
                else
                    tuple7 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5]) <*> (fromJSON a.[6])
            | a -> Failure (sprintf "Expected array, found %A" a)

    // Default, for external classes.
    type FromJSONClass with 
        static member inline FromJSON (r:'R, _:obj                             ) = (^R : (static member FromJSON: ^R   -> (JsonValue -> ^R ParseResult)) r ) : JsonValue ->  ^R ParseResult
        static member inline FromJSON (_:'R, _:Collections.IStructuralEquatable) = fun js -> (^R : (static member FromJSON: JsonValue -> ^R ParseResult) js) : ^R ParseResult

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