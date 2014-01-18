namespace Fleece

[<AutoOpen>]
module Fleece =

    open System
    open System.Globalization
    open System.Json
    open System.Collections.Generic
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
            | JsonType.Number -> JNumber (x.ReadAs<decimal>())
            | JsonType.Boolean -> JBool (x.ReadAs<bool>())
            | JsonType.String -> JString (x.ReadAs<string>())
            | JsonType.Default -> JNull
            | _ -> failwithf "Invalid JsonType %A for primitive %A" x.JsonType x
        | _ -> failwithf "Invalid JsonValue %A" o


    let inline JArray (x: JsonValue IReadOnlyList) = JsonArray x :> JsonValue
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonObject x :> JsonValue
    let inline JNumber (x: decimal) = JsonPrimitive x :> JsonValue
    let inline JBool (x: bool) = JsonPrimitive x :> JsonValue
    let inline JString (x: string) = JsonPrimitive x :> JsonValue
    let JNull : JsonValue = null

    // results:

    let (|Success|Failure|) =
        function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    // Deserializing:

    open FsControl
    open FsControl.Core
    open FSharpPlus

    type 'a ParseResult = Choice<'a, string>

    type FromJSON = FromJSON with
        static member instance (FromJSON, _: bool, _: bool ParseResult) = 
            function
            | JBool b -> Success b
            | a -> Failure (sprintf "Expected bool, actual %A" a)

        static member instance (FromJSON, _: string, _: string ParseResult) =
            function
            | JString b -> Success b
            | JNull -> Success null
            | a -> Failure (sprintf "Expected string, actual %A" a)

        static member instance (FromJSON, _: decimal, _: decimal ParseResult) =
            function
            | JNumber b -> Success b
            | a -> Failure (sprintf "Expected decimal, actual %A" a)

        static member instance (FromJSON, _: int, _: int ParseResult) =
            function
            | JNumber b ->
                if Decimal.Truncate b = b then
                    try
                        Success (int b)
                    with
                    | :? OverflowException -> Failure ("Int overflow: " + b.ToString(CultureInfo.InvariantCulture))
                else
                    Failure ("Invalid int " + b.ToString(CultureInfo.InvariantCulture))
            | a -> Failure (sprintf "Expected int, actual %A" a)

    let inline fromJSON (x: JsonValue) : 'a ParseResult = Inline.instance (FromJSON, Unchecked.defaultof<'a>) x

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

    type FromJSON with
        static member inline instance (FromJSON, _: Choice<'a, 'b>, _: Choice<'a, 'b> ParseResult) =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of2", a)] -> a |> fromJSON |> map Choice1Of2
                | [KeyValue("Choice2Of2", a)] -> a |> fromJSON |> map Choice2Of2
                | _ -> Failure (sprintf "Expected Choice, found %A" jobj)
            | a -> Failure (sprintf "Expected Choice, found %A" a)

        static member inline instance (FromJSON, _: Choice<'a, 'b, 'c>, _: Choice<'a, 'b, 'c> ParseResult) =
            function
            | JObject o as jobj ->
                match Seq.toList o with
                | [KeyValue("Choice1Of3", a)] -> a |> fromJSON |> map Choice1Of3
                | [KeyValue("Choice2Of3", a)] -> a |> fromJSON |> map Choice2Of3
                | [KeyValue("Choice3Of3", a)] -> a |> fromJSON |> map Choice3Of3
                | _ -> Failure (sprintf "Expected Choice, found %A" jobj)
            | a -> Failure (sprintf "Expected Choice, found %A" a)

        static member inline instance (FromJSON, _: 'a option, _: 'a option ParseResult) =
            function
            | JNull a -> Success None
            | x -> 
                let a: 'a ParseResult = fromJSON x
                map Some a

        static member inline instance (FromJSON, _: 'a array, _: 'a array ParseResult) =
            function
            | JArray a -> 
                let xx : 'a ParseResult seq = Seq.map fromJSON a
                sequenceA xx |> map Seq.toArray
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON,  _: 'a list, _: 'a list ParseResult) =
            function
            | JArray a -> 
                let xx : 'a ParseResult seq = Seq.map fromJSON a
                sequenceA xx |> map Seq.toList
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b, _: ('a * 'b) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 2 then
                    Failure ("Expected array with 2 items, was: " + x.ToString())
                else
                    tuple2 <!> (fromJSON a.[0]) <*> (fromJSON a.[1])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b * 'c, _: ('a * 'b * 'c) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 3 then
                    Failure ("Expected array with 3 items, was: " + x.ToString())
                else
                    tuple3 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b * 'c * 'd, _: ('a * 'b * 'c * 'd) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 4 then
                    Failure ("Expected array with 4 items, was: " + x.ToString())
                else
                    tuple4 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b * 'c * 'd * 'e, _: ('a * 'b * 'c * 'd * 'e) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 5 then
                    Failure ("Expected array with 5 items, was: " + x.ToString())
                else
                    tuple5 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b * 'c * 'd * 'e * 'f, _: ('a * 'b * 'c * 'd * 'e * 'f) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 6 then
                    Failure ("Expected array with 6 items, was: " + x.ToString())
                else
                    tuple6 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5])
            | a -> Failure (sprintf "Expected array, found %A" a)

        static member inline instance (FromJSON, _: 'a * 'b * 'c * 'd * 'e * 'f * 'g, _: ('a * 'b * 'c * 'd * 'e * 'f * 'g) ParseResult) =
            function
            | JArray a as x ->
                if a.Count <> 7 then
                    Failure ("Expected array with 7 items, was: " + x.ToString())
                else
                    tuple7 <!> (fromJSON a.[0]) <*> (fromJSON a.[1]) <*> (fromJSON a.[2]) <*> (fromJSON a.[3]) <*> (fromJSON a.[4]) <*> (fromJSON a.[5]) <*> (fromJSON a.[6])
            | a -> Failure (sprintf "Expected array, found %A" a)

    // Serializing:

    type ToJSON = ToJSON with
        static member instance (ToJSON, x: bool, _:JsonValue) = fun () -> JBool x
        static member instance (ToJSON, x: decimal, _:JsonValue) = fun () -> JNumber x
        static member instance (ToJSON, x: string, _:JsonValue) = fun () -> JString x
        static member instance (ToJSON, x: int, _:JsonValue) = fun () -> JNumber (decimal x)

    let inline toJSON (x: 'a) : JsonValue = Inline.instance (ToJSON, x) ()
    let jobj x = JObject ((dict x).AsReadOnlyDictionary())
    let inline jpair (key: string) value = key, toJSON value

    type ToJSON with
        static member inline instance (ToJSON, x: Choice<'a, 'b>, _:JsonValue) = fun () ->
            match x with
            | Choice1Of2 a -> jobj [ jpair "Choice1Of2" a ]
            | Choice2Of2 a -> jobj [ jpair "Choice2Of2" a ]

        static member inline instance (ToJSON, x: Choice<'a, 'b, 'c>, _:JsonValue) = fun () ->
            match x with
            | Choice1Of3 a -> jobj [ jpair "Choice1Of3" a ]
            | Choice2Of3 a -> jobj [ jpair "Choice2Of3" a ]
            | Choice3Of3 a -> jobj [ jpair "Choice3Of3" a ]

        static member inline instance (ToJSON, x: 'a option, _:JsonValue) = fun () ->
            match x with
            | None -> JNull
            | Some a -> toJSON a

        static member inline instance (ToJSON, x: 'a list, _:JsonValue) = fun () ->
            JArray ((List.map toJSON x).ToReadOnlyList())

        static member inline instance (ToJSON, x: 'a array, _:JsonValue) = fun () ->
            JArray ((Array.map toJSON x).AsReadOnlyList())

        static member inline instance (ToJSON, (a, b), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b|].AsReadOnlyList())

        static member inline instance (ToJSON, (a, b, c), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b; toJSON c|].AsReadOnlyList())

        static member inline instance (ToJSON, (a, b, c, d), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d|].AsReadOnlyList())

        static member inline instance (ToJSON, (a, b, c, d, e), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e|].AsReadOnlyList())

        static member inline instance (ToJSON, (a, b, c, d, e, f), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f|].AsReadOnlyList())

        static member inline instance (ToJSON, (a, b, c, d, e, f, g), _:JsonValue) = fun () ->
            JArray ([|toJSON a; toJSON b; toJSON c; toJSON d; toJSON e; toJSON f; toJSON g|].AsReadOnlyList())

    module Operators =
        let inline (.=) key value = jpair key value
        let inline (.@) o key = jget o key
        let inline (.@?) o key = jgetopt o key