namespace Fleece

[<AutoOpen>]
module Fleece =

    open System
    open System.Globalization
    open System.Json
    open System.Collections.Generic
    open FsControl
    open FsControl.Core
    open FSharpPlus
    open ReadOnlyCollectionsExtensions

    // pseudo-AST, wrapping JsonValue subtypes:

    let (|JArray|JObject|JNumber|JBool|JString|JNull|) (o: JsonValue) =
        match o with
        | null -> JNull
        | :? JsonArray as x ->
            let values = (x :> JsonValue IList).AsReadOnlyList()
            JArray values
        | :? JsonObject as x ->
            let values = (x :> IDictionary<string, JsonValue>).AsReadOnlyDictionary()
            JObject values
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

    // results:

    let (|Success|Failure|) =
        function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    // Deserializing:

    type 'a ParseResult = Choice<'a, string>

    let inline private failparse s v = Failure (sprintf "Expected %s, actual %A" s v)

    let inline private tryRead<'a> s = 
        function
        | JNumber j -> 
            match j.TryReadAs<'a>() with
            | true, v -> Success v
            | _ -> failparse s j
        | a -> failparse s a

    type FromJSONClass = FromJSONClass with
        static member FromJSON (_: bool) = 
            function
            | JBool b -> Success b
            | a -> failparse "bool" a

        static member FromJSON (_: string) =
            function
            | JString b -> Success b
            | JNull -> Success null
            | a -> failparse "string" a

        static member FromJSON (_: decimal) = tryRead<decimal> "decimal"
        static member FromJSON (_: int) = tryRead<int> "int"
        static member FromJSON (_: uint32) = tryRead<uint32> "uint32"
        static member FromJSON (_: int64) = tryRead<int64> "int64"
        static member FromJSON (_: uint64) = tryRead<uint64> "uint64"
        static member FromJSON (_: int16) = tryRead<int16> "int16"
        static member FromJSON (_: uint16) = tryRead<uint16> "uint16"
        static member FromJSON (_: char) =
            function
            | JString s -> 
                if s = null
                    then Failure "Expected char, got null"
                    else Success s.[0]
            | a -> failparse "char" a

        static member FromJSON (_: byte) = tryRead<byte> "byte"
        static member FromJSON (_: sbyte) = tryRead<sbyte> "sbyte"
        static member FromJSON (_: Double) = tryRead<Double> "double"
        static member FromJSON (_: Single) = tryRead<Single> "single"

        static member FromJSON (_: DateTime) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTime, got null"
                    else match DateTime.TryParseExact(s, "yyyy-MM-ddTHH:mm:ssZ", null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTime %s" s)
            | a -> failparse "DateTime" a

        static member FromJSON (_: DateTimeOffset) =
            function
            | JString s ->
                if s = null 
                    then Failure "Expected DateTimeOffset, got null"
                    else match DateTimeOffset.TryParseExact(s, "yyyy-MM-ddTHH:mm:ssK", null, DateTimeStyles.RoundtripKind) with
                         | true, t -> Success t
                         | _ -> Failure (sprintf "Invalid DateTimeOffset %s" s)
            | a -> failparse "DateTimeOffset" a

    module internal FromJSONOverloads =
        let inline instance (a: ^a, b: ^b) =
            ((^a or ^b) : (static member FromJSON: ^b -> (JsonValue -> ^b ParseResult)) b)

    let inline fromJSON (x: JsonValue) : 'a ParseResult = FromJSONOverloads.instance (FromJSONClass, Unchecked.defaultof<'a>) x

    let inline parseJSON (x: string) : 'a ParseResult =
        try
            let json = JsonValue.Parse x
            fromJSON json
        with e -> Failure (e.ToString())

    let inline jget (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> fromJSON value
        | _ -> Failure ("Key '" + key + "' not found in " + JObject(o).ToString())

    let inline jgetopt (o: IReadOnlyDictionary<string, JsonValue>) key =
        match o.TryGetValue key with
        | true, value -> fromJSON value |> map Some
        | _ -> Success None

    let inline private tuple2 a b = a,b
    let inline private tuple3 a b c = a,b,c
    let inline private tuple4 a b c d = a,b,c,d
    let inline private tuple5 a b c d e = a,b,c,d,e
    let inline private tuple6 a b c d e f = a,b,c,d,e,f
    let inline private tuple7 a b c d e f g = a,b,c,d,e,f,g

    type FromJSONClass with
        static member inline FromJSON (_: Choice<'a, 'b>) =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> fromJSON |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> fromJSON |> map Choice2Of2
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

        static member inline FromJSON (_: Choice<'a, 'b, 'c>) =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> fromJSON |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> fromJSON |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> fromJSON |> map Choice3Of3
                | _ -> failparse "Choice" jobj
            | a -> failparse "Choice" a

        static member inline FromJSON (_: 'a option) =
            function
            | JNull a -> Success None
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map Some a

        static member inline FromJSON (_: 'a array) =
            function
            | JArray a -> 
                let xx : 'a ParseResult seq = Seq.map fromJSON a
                sequenceA xx |> map Seq.toArray
            | a -> failparse "array" a

        static member inline FromJSON (_: 'a list) =
            function
            | JArray a -> 
                let xx : 'a ParseResult seq = Seq.map fromJSON a
                sequenceA xx |> map Seq.toList
            | a -> failparse "array" a

        static member inline FromJSON (_: 'a Set) =
            function
            | JArray a -> 
                let xx : 'a ParseResult seq = Seq.map fromJSON a
                sequenceA xx |> map set
            | a -> failparse "array" a

        static member inline FromJSON (_: 'a * 'b) =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + x.ToString())
                else
                    tuple2 <!> (fromJSON a.[0]) <*> (fromJSON a.[1])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline FromJSON (_: 'a * 'b * 'c) =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + x.ToString())
                else
                    tuple3 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline FromJSON (_: 'a * 'b * 'c * 'd) =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + x.ToString())
                else
                    tuple4 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e) =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + x.ToString())
                else
                    tuple5 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f) =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + x.ToString())
                else
                    tuple6 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline FromJSON (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g) =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + x.ToString())
                else
                    tuple7 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5]) <*> (fromJSON a.[6])
            | a -> Failure (sprintf "Expected array, found %A" a)

    // Serializing:

    type ToJSONClass = ToJSONClass with
        static member ToJSON (x: bool) = JBool x
        static member ToJSON (x: string) = JString x
        static member ToJSON (x: decimal) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: Double) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: Single) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: int) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: uint32) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: int64) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: uint64) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: int16) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: uint16) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: byte) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: sbyte) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: char) = JsonPrimitive x :> JsonValue
        static member ToJSON (x: DateTime) = JString (x.ToString("yyyy-MM-ddTHH:mm:ssZ")) // JsonPrimitive is incorrect for DateTime
        static member ToJSON (x: DateTimeOffset) = JString (x.ToString("yyyy-MM-ddTHH:mm:ssK")) // JsonPrimitive is incorrect for DateTimeOffset

    module internal ToJSONOverloads =
        let inline instance (a: ^a, b: ^b) =
            ((^a or ^b) : (static member ToJSON: ^b -> JsonValue) b)

    let inline toJSON (x: 'a) : JsonValue = ToJSONOverloads.instance (ToJSONClass, x)

    let jobj x = JObject ((dict x).AsReadOnlyDictionary())
    let inline jpair (key: string) value = key, toJSON value

    type ToJSONClass with
        static member inline ToJSON (x: Choice<'a, 'b>) =
            match x with
            | Choice1Of2 a -> jobj [ jpair "Choice1Of2" a ]
            | Choice2Of2 a -> jobj [ jpair "Choice2Of2" a ]

        static member inline ToJSON (x: Choice<'a, 'b, 'c>) =
            match x with
            | Choice1Of3 a -> jobj [ jpair "Choice1Of3" a ]
            | Choice2Of3 a -> jobj [ jpair "Choice2Of3" a ]
            | Choice3Of3 a -> jobj [ jpair "Choice3Of3" a ]

        static member inline ToJSON (x: 'a option) =
            match x with
            | None -> JNull
            | Some a -> toJSON a

        static member inline ToJSON (x: 'a list) =
            JArray ((List.map toJSON x).ToReadOnlyList())

        static member inline ToJSON (x: 'a Set) =
            JArray ((Seq.map toJSON x).ToReadOnlyList())

        static member inline ToJSON (x: 'a array) =
            JArray ((Array.map toJSON x).AsReadOnlyList())

        static member inline ToJSON ((a, b)) =
            JArray ([|toJSON a; toJSON b|].AsReadOnlyList())

        static member inline ToJSON ((a, b, c)) =
            JArray ([|toJSON a; toJSON b; toJSON c|].AsReadOnlyList())

        static member inline ToJSON ((a, b, c, d)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d|].AsReadOnlyList())

        static member inline ToJSON ((a, b, c, d, e)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e|].AsReadOnlyList())

        static member inline ToJSON ((a, b, c, d, e, f)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f|].AsReadOnlyList())

        static member inline ToJSON ((a, b, c, d, e, f, g)) =
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f; toJSON g|].AsReadOnlyList())

    module Operators =
        let inline (.=) key value = jpair key value
        let inline (.@) o key = jget o key
        let inline (.@?) o key = jgetopt o key