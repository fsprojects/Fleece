module Fleece.System.Json

    open Fleece
    open System
    open System.Globalization    
    open System.Collections.Generic
    open FSharpPlus
    open ReadOnlyCollectionsExtensions

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


    module Helpers =
        open Fleece.Helpers

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


// ===
