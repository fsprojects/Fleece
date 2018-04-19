module Fleece.FSharp.Data

    open Fleece
    open System
    open System.Globalization    
    open System.Collections.Generic
    open FSharpPlus
    open ReadOnlyCollectionsExtensions

    
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
                member __.Item with get(key:string) = properties |> Array.find (fun (k,v) -> k = key) |> snd                
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
    
    let dictAsProps (x: IReadOnlyDictionary<string, JsonValue>) : JsonObject = 
        match x with
        | :? ReadOnlyJsonPropertiesDictionary as x -> x.Properties
        | _ -> x |> Seq.map (fun p -> p.Key,p.Value) |> Array.ofSeq

    let inline JArray (x: JsonValue IReadOnlyList) = JsonValue.Array (x |> Array.ofSeq)
    let inline JObject (x: IReadOnlyDictionary<string, JsonValue>) = JsonValue.Record (dictAsProps x)
    let inline JBool (x: bool) = JsonValue.Boolean x
    let JNull : JsonValue = JsonValue.Null
    let inline JString (x: string) = 
        if x = null 
            then JsonValue.Null
            else JsonValue.String x


    module Helpers =
        open Fleece.Helpers

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

// ===