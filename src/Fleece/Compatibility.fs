#if FSHARPDATA
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
[<AutoOpen>]
type Extensions =
    static member Encoding x = Encoding.Wrap x

[<RequireQualifiedAccess>]
module JsonValue =
    let Parse (x: string) = let doc = System.Text.Json.JsonDocument.Parse x in doc.RootElement

type JsonValue = System.Text.Json.JsonElement
#endif

open Fleece
open Internals
open FSharpPlus
open FSharpPlus.Data


#if SYSTEMTEXTJSON
open Fleece.SystemTextJson.InternalHelpers
#endif


///////////////////////
// Main entry points //
///////////////////////

[<AutoOpen>]
type Operators =
    
    /// Gets the json encoding representation of the value, using its default codec.
    static member inline toJson (x: 'T) : Encoding = toEncoding<Encoding, 'T> x
    
    /// Attempts to decode the value from its json encoding representation, using its default codec.
    static member inline ofJson (x: Encoding) : Result<'T, DecodeError> = ofEncoding x
    
    /// Gets the native json type representation of the value, using its default codec.
    static member inline toJsonValue (x: 'T) : JsonValue = toEncoding<Encoding, 'T> x |> Encoding.Unwrap
    
    /// Attempts to decode the value from its native json type representation, using its default codec.
    static member inline ofJsonValue (x: JsonValue) : Result<'T, DecodeError> = ofEncoding (Encoding x)
    
    /// Gets the json text representation of the value, using its default codec.
    static member inline toJsonText (x: 'T) = x |> Operators.toJson |> string
        
    /// Attempts to decode the value from its json text representation, using its default codec.
    static member inline ofJsonText (x: string) : Result<'T, DecodeError> = try (Encoding.Parse x |> ofEncoding) with e -> Decode.Fail.parseError e x

    
///////////////////
// Compatibility //
///////////////////

[<CompiledName("Compatibility"); System.Obsolete("It will open the compatibility portion of the Operators module. Functions here will be removed in future versions. To use current functions, try removing this 'open' and make sure Fleece namespace is opened.", false)>]
module Operators =
    
    type JsonObject = Map<string, Encoding>
    type ParseResult<'t> = Result<'t, DecodeError>

    type Codec<'S1, 'S2, 't1, 't2> = Fleece.Codec<'S1, 'S2, 't1, 't2>
    type Codec<'S, 't> = Fleece.Codec<'S, 't>

    type DecodeError = Fleece.DecodeError

    let (|JsonTypeMismatch|_|) = function
        | Fleece.DecodeError.EncodingCaseMismatch (destinationType, encodedValue, expectedCase, actualCase) -> Some (destinationType, encodedValue, expectedCase, actualCase)
        | _ -> None

    let (|NullString|_|) = function
        | Fleece.DecodeError.NullString destinationType -> Some destinationType
        | _ -> None

    let (|IndexOutOfRange|_|) = function
        | Fleece.DecodeError.IndexOutOfRange (int, iEncoding) -> Some (int, iEncoding)
        | _ -> None

    let (|InvalidValue|_|) = function
        | Fleece.DecodeError.InvalidValue (destinationType, iEncoding, additionalInformation) -> Some (destinationType, iEncoding, additionalInformation)
        | _ -> None
    
    let (|PropertyNotFound|_|) = function
        | Fleece.DecodeError.PropertyNotFound (string, obj) -> Some (string, obj)
        | _ -> None

    let (|ParseError|_|) = function
        | Fleece.DecodeError.ParseError (destinationType, exn, string) -> Some (destinationType, exn, string)
        | _ -> None

    let (|Uncategorized|_|) = function
       | Fleece.DecodeError.Uncategorized string -> Some string
       | _ -> None

    let (|Multiple|_|) = function
        | Fleece.DecodeError.Multiple list -> Some list
        | _ -> None

    
    module Decode =
        let inline Success x = Ok x : ParseResult<_>
        let (|Success|Failure|) (x: ParseResult<_>) = x |> function
            | Ok    x -> Success x
            | Error x -> Failure x
    
        module Fail =
            let inline objExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (EncodingCaseMismatch (typeof<'t>, v, "Object", a))
            let inline arrExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (EncodingCaseMismatch (typeof<'t>, v, "Array" , a))
            let inline numExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (EncodingCaseMismatch (typeof<'t>, v, "Number", a))
            let inline strExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (EncodingCaseMismatch (typeof<'t>, v, "String", a))
            let inline boolExpected (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (EncodingCaseMismatch (typeof<'t>, v, "Bool"  , a))
            let [<GeneralizableValue>]nullString<'t> : Result<'t, _> = Error (NullString typeof<'t>)
            let inline count e (x: 'Encoding) =
                let a =
                    match (Codecs.array (Ok <-> id) |> Codec.decode) x with
                    | Ok a -> a
                    | Error x -> failwithf "Error on error handling: Expected an 'Encoding [] but received %A." x
                Error (IndexOutOfRange (e, map (fun x -> x :> IEncoding) a))
            let invalidValue (v: 'Encoding) o : Result<'t, _> = Error (InvalidValue (typeof<'t>, v, o))
            let propertyNotFound p (o: PropertyList<'Encoding>) = Error (PropertyNotFound (p, map (fun x -> x :> IEncoding) o))
            let parseError s v : Result<'t, _> = Error (ParseError (typeof<'t>, s, v))

    
    /// Functions operating on Codecs
    module Codec =
    
        let decode { Decoder = d } = d
        let encode { Encoder = e } = e
    
        /// Turns a Codec into another Codec, by mapping it over an isomorphism.
        let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) c =
            let { Decoder = r ; Encoder = w } = c
            contramap f r <-> map g w
    
    
        /// Creates a new codec which is the result of applying codec2 then codec1 for encoding
        /// and codec1 then codec2 for decoding
        let inline compose codec1 codec2 =
            let { Decoder = dec1 ; Encoder = enc1 } = codec1
            let { Decoder = dec2 ; Encoder = enc2 } = codec2
            (dec1 >> (=<<) dec2) <-> (enc1 << enc2)
    
        /// Maps a function over the decoder.
        let map (f: 't1 -> 'u1) (field: Codec<PropertyList<'S>, PropertyList<'S>, 't1, 't2>) =
            {
                Decoder = fun x ->
                    match field.Decoder x with
                    | Error e -> Error e
                    | Ok a    -> Ok (f a)
    
                Encoder = field.Encoder
            }
    
        
        let ofConcrete x = id x
        let toConcrete x = id x    
    
    [<RequireQualifiedAccess>]
    module JsonCodec =
        let result (x: Codec<Encoding, _>) = Codecs.result x
        let choice (x: Codec<Encoding, _>) = Codecs.choice x
        let choice3 (x: Codec<Encoding, _>) = Codecs.choice3 x
        let option (x: Codec<Encoding, _>) = Codecs.option x
        let nullable (x: Codec<Encoding, _>) = Codecs.nullable x
        let array (x: Codec<Encoding, _>) = Codecs.array x
        let arraySegment (x: Codec<Encoding, _>) = Codecs.arraySegment x
        let list (x: Codec<Encoding, _>) = Codecs.list x
        let set (x: Codec<Encoding, _>) = Codecs.set x
        let resizeArray (x: Codec<Encoding, _>) = Codecs.resizeArray x
        let map         (x: Codec<Encoding, _>) = Codecs.propMap x
        let dictionary  (x: Codec<Encoding, _>) = Codecs.propDictionary x
        let unit ()  : Codec<Encoding, _>  = Codecs.unit
        let tuple2 (x: Codec<Encoding, _>) = Codecs.tuple2 x
        let tuple3 (x: Codec<Encoding, _>) = Codecs.tuple3 x
        let tuple4 (x: Codec<Encoding, _>) = Codecs.tuple4 x
        let tuple5 (x: Codec<Encoding, _>) = Codecs.tuple5 x
        let tuple6 (x: Codec<Encoding, _>) = Codecs.tuple6 x
        let tuple7 (x: Codec<Encoding, _>) = Codecs.tuple7 x
        let boolean        : Codec<Encoding, _> = Codecs.boolean
        let string         : Codec<Encoding, _> = Codecs.string
        let dateTime       : Codec<Encoding, _> = Codecs.dateTime
        let dateTimeOffset : Codec<Encoding, _> = Codecs.dateTimeOffset
        let decimal        : Codec<Encoding, _> = Codecs.decimal
        let float          : Codec<Encoding, _> = Codecs.float
        let float32        : Codec<Encoding, _> = Codecs.float32
        let int            : Codec<Encoding, _> = Codecs.int
        let uint32         : Codec<Encoding, _> = Codecs.uint32
        let int64          : Codec<Encoding, _> = Codecs.int64
        let uint64         : Codec<Encoding, _> = Codecs.uint64
        let int16          : Codec<Encoding, _> = Codecs.int16
        let uint16         : Codec<Encoding, _> = Codecs.uint16
        let byte           : Codec<Encoding, _> = Codecs.byte
        let sbyte          : Codec<Encoding, _> = Codecs.sbyte
        let char           : Codec<Encoding, _> = Codecs.char
        let guid           : Codec<Encoding, _> = Codecs.guid
    
    
    let inline jsonValueCodec< ^t when (GetCodec or  ^t) : (static member GetCodec :  ^t * GetCodec * GetCodec * OpCodec -> Codec<Encoding, ^t>)> = GetCodec.Invoke<Encoding, OpCodec, 't> Unchecked.defaultof<'t>

    let jobj (x: list<string * Encoding>) : Encoding = x |> List.toArray |> PropertyList |> Codec.encode (Codecs.propList Codecs.id)

    let JNull     : Encoding = (Codecs.option Codecs.unit |> Codec.encode) None
    let JBool   x : Encoding = (Codecs.boolean |> Codec.encode) x
    let JNumber x : Encoding = (Codecs.decimal |> Codec.encode) x
    let JString x : Encoding = (Codecs.string  |> Codec.encode) x
    let JArray (x: System.Collections.Generic.IReadOnlyList<Encoding>) : Encoding = (Codecs.array (Ok <-> id) |> Codec.encode) (toArray x)
    let JObject (x: PropertyList<Encoding>) : Encoding = (Codecs.propList (Ok <-> id) |> Codec.encode) x
    
    let (|JNull|_|)   (x: Encoding) = match (Codecs.option (Ok <-> id) |> Codec.decode) x with | Ok None -> Some () | _ -> None    
    let (|JBool|_|)   (x: Encoding) = (Codecs.boolean |> Codec.decode) x |> Option.ofResult
    let (|JNumber|_|) (x: Encoding) = (Codecs.decimal |> Codec.decode) x |> Option.ofResult
    let (|JString|_|) (x: Encoding) = (Codecs.string  |> Codec.decode) x |> Option.ofResult
    let (|JArray|_|)  (x: Encoding) = (Codecs.array        (Ok <-> id) |> Codec.decode) x |> Option.ofResult |> Option.map IReadOnlyList.ofArray
    let (|JObject|_|) (x: Encoding) = (Codecs.propList (Ok <-> id) |> Codec.decode) x |> Option.ofResult

    /// A codec to encode a collection of property/values into a Json encoding and the other way around.
    let jsonObjToValueCodec : Codec<Encoding, PropertyList<Encoding>> = ((function JObject (o: PropertyList<_>) -> Ok o | a -> Decode.Fail.objExpected a) <-> JObject)

    /// A codec to encode a Json value to a Json text and the other way around.
    let jsonValueToTextCodec = (fun x -> try Ok (Encoding.Parse x) with e -> Decode.Fail.parseError e x) <-> (fun (x: Encoding) -> string x)

    let inline parseJson (x: string) : ParseResult<'T> = Codec.decode jsonValueToTextCodec x >>= Operators.ofJson

    let inline jreq name getter = jreq name getter : Codec<PropertyList<Encoding>,_,_,_>
    let inline jopt name getter = jopt name getter : Codec<PropertyList<Encoding>,_,_,_>
    
    let inline jreqWith codec name getter = jreqWith codec name getter : Codec<PropertyList<Encoding>,_,_,_>
    let inline joptWith codec name getter = joptWith codec name getter : Codec<PropertyList<Encoding>,_,_,_>

    let inline jchoice (codecs: seq<Codec<PropertyList<Encoding>, PropertyList<Encoding>, 't1, 't2>>) =
        let head, tail = Seq.head codecs, Seq.tail codecs
        foldBack (<|>) tail head

        

    /// Gets a value from a Json object
    let jgetWith ofJson (o: PropertyList<Encoding>) key =
        match o.[key] with
        | value::_ -> ofJson value
        | _ -> Decode.Fail.propertyNotFound key (o |> map (fun x -> x :> IEncoding))

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let jgetOptWith ofJson (o: PropertyList<Encoding>) key =
        match o.[key] with
        | JNull _::_ -> Ok None
        | value  ::_ -> ofJson value |> Result.map Some
        | _ -> Ok None

    /// Gets a value from a Json object
    let inline jget (o: PropertyList<Encoding>) key = jgetWith ofEncoding o key

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline jgetOpt (o: PropertyList<Encoding>) key = jgetOptWith ofEncoding o key

    /// Gets a value from a Json object
    let inline (.@) o key = jget o key

    /// Tries to get a value from a Json object.
    /// Returns None if key is not present in the object.
    let inline (.@?) o key = jgetOpt o key

    /// Creates a new Json key-value pair for a Json object
    let inline jpairWith toJson (key: string) value = key, toJson value

    /// Creates a new Json key-value pair for a Json object
    let inline jpair (key: string) value = jpairWith toEncoding key value

    /// Creates a new Json key-value pair for a Json object if the value option is present
    let jpairOptWith toJson (key: string) value = match value with Some value -> (key, toJson value) | _ -> (null, JNull)

    /// Creates a new Json key-value pair for a Json object if the value option is present
    let inline jpairOpt (key: string) value = jpairOptWith toJson key value


    /// Creates a new Json key-value pair for a Json object
    let inline (.=) key value = jpair key value

    /// Creates a new Json key-value pair for a Json object if the value is present in the option
    let inline (.=?) (key: string) value = jpairOpt key value

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