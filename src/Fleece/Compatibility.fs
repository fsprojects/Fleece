﻿#if FSHARPDATA
namespace Fleece.FSharpData
type JsonValue = FSharp.Data.JsonValue
#endif

#if NEWTONSOFT
namespace Fleece.Newtonsoft
#endif

#if SYSTEMJSON
namespace Fleece.SystemJson
type JsonValue = System.Json.JsonValue
#endif

#if SYSTEMTEXTJSON
namespace Fleece.SystemTextJson
[<AutoOpen>]type X = static member Encoding x = x
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Encoding =
    let Parse x = Fleece.SystemTextJson.Encoding.Parse x

type JsonValue = Encoding
#endif

open Fleece
open Internals
open FSharpPlus
open FSharpPlus.Data
open Fleece.Helpers
open Fleece.Operators

#if SYSTEMTEXTJSON
open Internal
#endif

// Main + Backwards compatibility functions
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Operators =
    
    ///////////////////////
    // Main entry points //
    ///////////////////////
    
    /// Gets the json encoding representation of the value, using its default codec.
    let inline toJson (x: 'T) : Encoding = toEncoding<Encoding, 'T> x
    
    /// Attempts to decode the value from its json encoding representation, using its default codec.
    let inline ofJson (x: Encoding) : Result<'T, DecodeError> = ofEncoding x
    
    /// Gets the json value representation of the value, using its default codec.
    let inline toJsonValue (x: 'T) : JsonValue = toEncoding<Encoding, 'T> x |> Encoding.Unwrap
    
    /// Attempts to decode the value from its json value representation, using its default codec.
    let inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (Encoding x)
    
    /// Gets the json text representation of the value, using its default codec.
    let inline toJsonText (x: 'T) = x |> toJson |> string
        
    /// Attempts to decode the value from its json text representation, using its default codec.
    let inline ofJsonText (x: string) : Result<'T, DecodeError> = try (Encoding.Parse x |> ofEncoding) with e -> Decode.Fail.parseError e x

    
    ///////////////////
    // Compatibility //
    ///////////////////    
    
    type JsonObject = Map<string, Encoding>

    let inline jsonValueCodec< ^t when (GetCodec or  ^t) : (static member GetCodec :  ^t * GetCodec * OpCodec -> Codec<Encoding, ^t>)> = GetCodec.Invoke<Encoding, OpCodec, 't> Unchecked.defaultof<'t>

    let jobj (x: list<string * Encoding>) : Encoding =
        let (Codec (_, enc)) = Codecs.multiMap (Ok <-> id)
        multiMap (x |> Seq.map System.Collections.Generic.KeyValuePair)
        |> enc

    let JString x = Encoding (JString x)

    let JObject x =
        (Codecs.multiMap (Ok <-> id)
        |> Codec.encode) x

    let (|JObject|_|) (x: Encoding) =
        (Codecs.multiMap (Ok <-> id)
        |> Codec.decode) x
        |> Option.ofResult

    let (|JNull|_|) (x: Encoding) =
        let (Codec (dec, _)) = Codecs.option (Ok <-> id)
        match dec x with
        | Ok x when Option.isNone x -> Some ()
        | _ -> None

    let (|JString|_|) (x: Encoding) =
        let (Codec (dec, _)) = Codecs.string
        dec x |> Option.ofResult

    /// A codec to encode a collection of property/values into a Json encoding and the other way around.
    let jsonObjToValueCodec : Codec<Encoding, MultiObj<Encoding>> = ((function JObject (o: MultiMap<_,_>) -> Ok o | a -> Decode.Fail.objExpected a) <-> JObject)

    /// A codec to encode a Json value to a Json text and the other way around.
    let jsonValueToTextCodec = (fun x -> try Ok (Encoding.Parse x) with e -> Decode.Fail.parseError e x) <-> (fun (x: Encoding) -> string x)

    let inline parseJson (x: string) : ParseResult<'T> = Codec.decode jsonValueToTextCodec x >>= ofJson

    let inline jreq name getter = jreq name getter : Codec<MultiObj<Encoding>,_,_,_>
    let inline jopt name getter = jopt name getter : Codec<MultiObj<Encoding>,_,_,_>
    
    let inline jreqWith codec name getter = jreqWith codec name getter : Codec<MultiObj<Encoding>,_,_,_>
    let inline joptWith codec name getter = joptWith codec name getter : Codec<MultiObj<Encoding>,_,_,_>

    let inline jchoice (codecs: seq<Codec<MultiObj<Encoding>, MultiObj<Encoding>, 't1, 't2>>) =
        let head, tail = Seq.head codecs, Seq.tail codecs
        foldBack (<|>) tail head

        

    /// Gets a value from a Json object
    let jgetWith ofJson (o: MultiObj<Encoding>) key =
        match o.[key] with
        | value::_ -> ofJson value
        | _ -> Decode.Fail.propertyNotFound key (o |> MultiMap.mapValues (fun x -> x :> IEncoding))

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let jgetOptWith ofJson (o: MultiObj<Encoding>) key =
        match o.[key] with
        | JNull _::_ -> Ok None
        | value  ::_ -> ofJson value |> Result.map Some
        | _ -> Ok None

    /// Gets a value from a Json object
    let inline jget (o: MultiObj<Encoding>) key = jgetWith ofEncoding o key

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline jgetOpt (o: MultiObj<Encoding>) key = jgetOptWith ofEncoding o key

    /// Gets a value from a Json object
    let inline (.@) o key = jget o key

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline (.@?) o key = jgetOpt o key

    /// Creates a new Json key-value pair for a Json object
    let inline jpairWith toJson (key: string) value = key, toJson value

    /// Creates a new Json key-value pair for a Json object
    let inline jpair (key: string) value = jpairWith toEncoding key value


    /// Creates a new Json key-value pair for a Json object
    let inline (.=) key value = jpair key value

    let jsonObjectGetValues x = id x


    // Verbose syntax

    /// <summary>Initialize the field mappings.</summary>
    /// <param name="f">An object constructor as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline withFields f : Codec<'s,'s,_,_> = result f //(fun _ -> Ok f) <-> (fun _ -> multiMap [])
        
    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfield fieldName getter rest = rest <*> jreq fieldName (getter >> Some)
        
    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldOpt fieldName getter rest = rest <*> jopt fieldName getter

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldWith codec fieldName getter rest = rest <*> jreqWith codec fieldName (getter >> Some)

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="codec">The codec thunk to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldWithLazy codec fieldName getter rest = rest <*> jreqWithLazy codec fieldName (getter >> Some)

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jfieldOptWith codec fieldName getter rest = rest <*> joptWith codec fieldName getter


module Lens =
    open FSharpPlus.Lens
    let inline _JString x = (prism' JString <| function JString s -> Some s | _ -> None) x
    let inline _JObject x = (prism' JObject <| function JObject s -> Some s | _ -> None) x
    let inline _JArray  x = (prism' JArray  <| function JArray  s -> Some s | _ -> None) x
    let inline _JBool   x = (prism' JBool   <| function JBool   s -> Some s | _ -> None) x
    let inline _JNumber x = (prism' JNumber <| fun v -> match Operators.ofJsonValue v : decimal ParseResult with Ok s -> Some s | _ -> None) x
    let inline _JNull   x = prism' (konst JNull) (function JNull -> Some () | _ -> None) x

    /// Like '_jnth', but for 'Object' with Text indices.
    let inline _jkey i =
        let inline dkey i f t = map (fun x -> IReadOnlyDictionary.add i x t) (f (IReadOnlyDictionary.tryGetValue i t |> Option.defaultValue JNull))
        _JObject << dkey i

    let inline _jnth i =
        let inline dnth i f t = map (fun x -> t |> IReadOnlyList.trySetItem i x |> Option.defaultValue t) (f (IReadOnlyList.tryItem i t |> Option.defaultValue JNull))
        _JArray << dnth i

    // Reimport some basic Lens operations from F#+

    let setl optic value   (source: 's) : 't = setl optic value source
    let over optic updater (source: 's) : 't = over optic updater source
    let preview (optic: ('a -> Const<_,'b>) -> _ -> Const<_,'t>) (source: 's) : 'a option = preview optic source