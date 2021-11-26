namespace Fleece

#nowarn "00042"

open System
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data

type Id1<'t> (v: 't) =
    let value = v
    member __.getValue = value

type Id2<'t> (v: 't) =
    let value = v
    member __.getValue = value


type OvCodecError = interface end
type OvDecEncError = interface inherit OvCodecError end

type IDefaultA = interface inherit OvDecEncError end
type IDefault9 = interface inherit IDefaultA end
type IDefault8 = interface inherit IDefault9 end
type IDefault7 = interface inherit IDefault8 end
type IDefault6 = interface inherit IDefault7 end
type IDefault5 = interface inherit IDefault6 end
type IDefault4 = interface inherit IDefault5 end
type IDefault3 = interface inherit IDefault4 end
type IDefault2 = interface inherit IDefault3 end
type IDefault1 = interface inherit IDefault2 end

type OpCodec = OpCodec
type OpEncode = OpEncode
type OpDecode = OpDecode



/// Marker interface for all interfaces whose derived classes will support codecs
type ICodecInterface<'Base> = interface end

module Config =
    let mutable codecCacheEnabled = true

module Helpers =

    let inline retype (a: 'a) : 'b =
    #if !FABLE_COMPILER
        (# "" a : 'b #)
    #else
        unbox<'b> a
    #endif


    #if !FABLE_COMPILER
    module ArraySegment =
        let toArray (x: ArraySegment<'a>) =
            if isNull x.Array then invalidOp "Null Array" else
            if x.Count = 0 then Array.empty else
            let array = Array.zeroCreate<'a> x.Count
            System.Array.Copy (x.Array, x.Offset, array, 0, x.Count)
            array
    #endif

    /// Creates a MultiMap from a seq of KeyValue pairs.
    let multiMap (x: seq<KeyValuePair<_, _>>) = x |> Seq.map (|KeyValue|) |> MultiMap.ofSeq

    module Dictionary =
        open System.Collections.Generic

        let ofSeq (source: seq<'Key * 'T>) =
            let dct = Dictionary ()
            for (k, v) in source do
                dct.Add (k, v)
            dct

        let toSeq (source: Dictionary<'Key, 'T>) = seq {
            for (KeyValue (k, v)) in source do
                yield (k, v) }

    let decoderNotAvailable (_: 'Encoding) : Result<'T, _> = failwithf "Fleece internal error: this codec has no decoder from encoding %A to type %A." typeof<'Encoding> typeof<'T>
    let encoderNotAvailable (_: 'T) : 'Encoding            = failwithf "Fleece internal error: this codec has no encoder from type %A to encoding %A." typeof<'T> typeof<'Encoding>

open Helpers


/// Encodes a value of a generic type 't into a value of raw type 'S.
type Encoder<'S, 't> = 't -> 'S


/// An alias for a MultimMap with string keys
type PropertyList<'t> = MultiMap<string, 't>

/// A decoder from raw type 'S1 and encoder to raw type 'S2 for strong types 't1 and 't2.
type Codec<'S1, 'S2, 't1, 't2> = { Decoder : Decoder<'S1, 't1>; Encoder : Encoder<'S2, 't2> } with
    static member inline Return f = { Decoder = (fun _ -> Ok f); Encoder = zero }


/// A codec for raw type 'S to strong type 't.
and Codec<'S, 't> = Codec<'S, 'S, 't, 't>

/// Decodes a value of raw type 'S into a value of generic type 't, possibly returning an error.
and Decoder<'S, 't> = 'S -> Result<'t, DecodeError>

and ParseResult<'t> = Result<'t, DecodeError>

and IEncoding =
    inherit IDefault1

    abstract unit           : Codec<IEncoding, unit>
    abstract boolean        : Codec<IEncoding, bool>
    abstract string         : Codec<IEncoding, string>
    abstract dateTime       : Codec<IEncoding, DateTime>
    abstract dateTimeOffset : Codec<IEncoding, DateTimeOffset>
    abstract timeSpan       : Codec<IEncoding, TimeSpan>
    abstract decimal        : Codec<IEncoding, Decimal>
    abstract float          : Codec<IEncoding, float>
    abstract float32        : Codec<IEncoding, float32>
    abstract int            : Codec<IEncoding, int>
    abstract uint32         : Codec<IEncoding, uint32>
    abstract int64          : Codec<IEncoding, int64>
    abstract uint64         : Codec<IEncoding, uint64>
    abstract int16          : Codec<IEncoding, int16>
    abstract uint16         : Codec<IEncoding, uint16>
    abstract byte           : Codec<IEncoding, byte>
    abstract sbyte          : Codec<IEncoding, sbyte>
    abstract char           : Codec<IEncoding, char>
    abstract guid           : Codec<IEncoding, Guid>
    abstract result         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, Result<'t1,'t2>>
    abstract choice         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, Choice<'t1,'t2>>
    abstract choice3        : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, Choice<'t1,'t2,'t3>>
    abstract option         : Codec<IEncoding, 't>  -> Codec<IEncoding, option<'t>>
    abstract array          : Codec<IEncoding, 't>  -> Codec<IEncoding, 't []>
    abstract multiMap       : Codec<IEncoding, 't>  -> Codec<IEncoding, PropertyList<'t>>
    abstract tuple1         : Codec<IEncoding, 't>  -> Codec<IEncoding, Tuple<'t>>
    abstract tuple2         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't1 * 't2>
    abstract tuple3         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't1 * 't2 * 't3>
    abstract tuple4         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4>
    abstract tuple5         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5>
    abstract tuple6         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't6> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5 * 't6>
    abstract tuple7         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't6> -> Codec<IEncoding, 't7> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5 * 't6 * 't7>
    
    // requires F# 5 --> 
    abstract enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> : Codec<IEncoding, 'u> -> Codec<IEncoding, 't>

    /// Returns a string representing the internal "case" (or type) of the encoding (ie: Array, Object, ... )
    abstract getCase    : string


and DecodeError =
    | EncodingCaseMismatch of DestinationType: Type * EncodedValue: IEncoding * ExpectedCase: string * ActualCase: string
    | NullString of DestinationType: Type
    | IndexOutOfRange of int * IEncoding
    | InvalidValue of DestinationType:  Type * EncodedValue: IEncoding * AdditionalInformation: string
    | PropertyNotFound of string * PropertyList<IEncoding>
    | ParseError of DestinationType: Type * exn * string
    | Uncategorized of string
    | Multiple of DecodeError list
with
    static member (+) (x, y) =
        match x, y with
        | Multiple x, Multiple y -> Multiple (x @ y)
        | Multiple x,  y         -> Multiple (x @ [y])
        | x, Multiple  y         -> Multiple (x::y)
        | _                      -> Multiple [x; y]
    override x.ToString () =
        match x with
        | EncodingCaseMismatch (t, v: IEncoding, expected, actual) -> sprintf "%s expected but got %s while decoding %s as %s" (string expected) (string actual) (string v) (string t)
        | NullString t            -> sprintf "Expected %s, got null" (string t)
        | IndexOutOfRange (e, a)  -> sprintf "Expected array with %s items, was: %s" (string e) (string a)
        | InvalidValue (t, v, s)  -> sprintf "Value %s is invalid for %s%s" (string v) (string t) (if String.IsNullOrEmpty s then "" else " " + s)
        | PropertyNotFound (p, o) -> sprintf "Property: '%s' not found in object '%s'" p (string o)
        | ParseError (t, s, v)    -> sprintf "Error decoding %s from  %s: %s" (string v) (string t) (string s)
        | Uncategorized str       -> str
        | Multiple lst            -> List.map string lst |> String.concat "\r\n"


[<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
module Internal =
    let (<->) decoder encoder : Codec<_,_,_,_> = { Decoder = decoder; Encoder = encoder }

open Internal

/// Functions operating on Codecs
module Codec =

    let decode { Decoder = d } = d
    let encode { Encoder = e } = e

    /// Turns a Codec into another Codec, by mapping it over an isomorphism.
    let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) c =
        let { Decoder = r ; Encoder = w } = c
        contramap f r <-> map g w

    let inline lift2 (f: 'x1 -> 'x2 -> 'r) (x1: Codec<'S, 'S, 'x1, 'T>) (x2: Codec<'S, 'S, 'x2, 'T>) : Codec<'S, 'S, 'r, 'T> =
        {
            Decoder = lift2 f x1.Decoder x2.Decoder
            Encoder = fun w -> (x1.Encoder w *> x2.Encoder w)
        }

    let inline lift3 f (x1: Codec<'S, 'S, 'x1, 'T>) (x2: Codec<'S, 'S, 'x2, 'T>) (x3: Codec<'S, 'S, 'x3, 'T>) : Codec<'S, 'S, 'r, 'T> =
        {
            Decoder = lift3 f x1.Decoder x2.Decoder x3.Decoder
            Encoder = fun w -> (x1.Encoder w *> x2.Encoder w *> x3.Encoder w)
        }

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

    let downCast<'t, 'S when 'S :> IEncoding> (x: Codec<IEncoding, 't> ) : Codec<'S, 't> =
        {
            Decoder = fun (p: 'S) -> x.Decoder (p :> IEncoding)
            Encoder = fun (p: 't) -> x.Encoder p :?> 'S
        }

    let upCast<'t, 'S when 'S :> IEncoding> (x: Codec<'S, 't>) : Codec<IEncoding, 't> =
        {
            Decoder = fun (p: IEncoding) -> x.Decoder (p :?> 'S)
            Encoder = fun (p: 't) -> x.Encoder p :> IEncoding
        }

    
    [<Obsolete("This function is no longer needed. You can safely remove it.")>]
    let ofConcrete x = id x

    [<Obsolete("This function is no longer needed. You can safely remove it.")>]
    let toConcrete x = id x


type Codec<'S1, 'S2, 't1, 't2> with

    static member (<.<) (c1, c2) = Codec.compose c1 c2
    static member (>.>) (c1, c2) = Codec.compose c2 c1
    
    static member (<*>) (remainderFields: Codec<PropertyList<'S>, PropertyList<'S>, 'f ->'r, 'T>, currentField: Codec<PropertyList<'S>, PropertyList<'S>, 'f, 'T>) =
        {
            Decoder = fun x ->
                match remainderFields.Decoder x, lazy (currentField.Decoder x) with
                | Error e, _ | _, Lazy (Error e) -> Error e
                | Ok a   , Lazy (Ok b)           -> Ok (a b)

            Encoder = fun t -> remainderFields.Encoder t ++ currentField.Encoder t
        }

    /// Apply two codecs in such a way that the field values are ignored when decoding.
    static member ( *>) (f: Codec<PropertyList<'S>, PropertyList<'S>, 't, 'u>, x) = f *> x : Codec<PropertyList<'S>, 'u>

    /// Apply two codecs in such a way that the field values are ignored when decoding.
    static member (<* )  (x, f: Codec<PropertyList<'S>, PropertyList<'S>, 't, 'u>) = x <* f : Codec<PropertyList<'S>, 'u>

    static member (<!>) (f, field: Codec<PropertyList<'S>, PropertyList<'S>, 'f, 'T>) = Codec.map f field
    static member Map   (field: Codec<PropertyList<'S>, PropertyList<'S>, 'f, 'T>, f) = Codec.map f field

    static member (<|>) (source: Codec<PropertyList<'S>, PropertyList<'S>, 'f, 'T>, alternative: Codec<PropertyList<'S>, PropertyList<'S>, 'f, 'T>) =
        {
            Decoder = fun r ->
                match source.Decoder r, lazy (alternative.Decoder r) with
                | Ok x, _ -> Ok x
                | Error x, Lazy (Error y) -> Error (x ++ y)
                | _, Lazy d -> d

            Encoder = fun t -> source.Encoder t ++ alternative.Encoder t
        }

    static member Lift2 (f: 'x ->'y ->'r, x: Codec<PropertyList<'S>, PropertyList<'S>,'x,'T>, y: Codec<PropertyList<'S>, PropertyList<'S>,'y,'T>) : Codec<PropertyList<'S>, PropertyList<'S>,'r,'T> =
        {
            Decoder = fun s -> ( (f <!> x.Decoder s : ParseResult<'y -> 'r>) <*> y.Decoder s : ParseResult<'r> )
            Encoder = x.Encoder ++ y.Encoder
        }






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
        let inline count e (a: 'Encoding) = Error (IndexOutOfRange (e, a))
        let invalidValue (v: 'Encoding) o : Result<'t, _> = Error (InvalidValue (typeof<'t>, v, o))
        let propertyNotFound p (o: PropertyList<'Encoding>) = Error (PropertyNotFound (p, map (fun x -> x :> IEncoding) o))
        let parseError s v : Result<'t, _> = Error (ParseError (typeof<'t>, s, v))






module Codecs =

    let private instance<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = Unchecked.defaultof<'Encoding>

    let unit<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.unit           |> Codec.downCast : Codec<'Encoding, _>
    let boolean<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.boolean        |> Codec.downCast : Codec<'Encoding, _>
    let guid<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.guid           |> Codec.downCast : Codec<'Encoding, _>
    let char<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.char           |> Codec.downCast : Codec<'Encoding, _>
    let byte<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.byte           |> Codec.downCast : Codec<'Encoding, _>
    let sbyte<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.sbyte          |> Codec.downCast : Codec<'Encoding, _>
    let uint16<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint16         |> Codec.downCast : Codec<'Encoding, _>
    let uint32<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint32         |> Codec.downCast : Codec<'Encoding, _>
    let uint64<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint64         |> Codec.downCast : Codec<'Encoding, _>
    let int16<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.int16          |> Codec.downCast : Codec<'Encoding, _>
    let int<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>            = instance<'Encoding>.int            |> Codec.downCast : Codec<'Encoding, _>
    let int64<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.int64          |> Codec.downCast : Codec<'Encoding, _>
    let decimal<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.decimal        |> Codec.downCast : Codec<'Encoding, _>
    let float32<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.float32        |> Codec.downCast : Codec<'Encoding, _>
    let float<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.float          |> Codec.downCast : Codec<'Encoding, _>
    let string<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.string         |> Codec.downCast : Codec<'Encoding, _>
    let dateTime<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>       = instance<'Encoding>.dateTime       |> Codec.downCast : Codec<'Encoding, _>
    let dateTimeOffset<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = instance<'Encoding>.dateTimeOffset |> Codec.downCast : Codec<'Encoding, _>
    let timeSpan<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>       = instance<'Encoding>.timeSpan       |> Codec.downCast : Codec<'Encoding, _>
    let array       (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.array (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, array<'a>>
    let list        (codec: Codec<'Encoding, 'a>) = (Ok << Array.toList <-> Array.ofList) >.> array codec
    let nelist      (codec: Codec<'Encoding, 'a>) = (Array.toList >> NonEmptyList.tryOfList >> Option.toResultWith (DecodeError.Uncategorized "List is empty") <-> Array.ofSeq) >.> array codec
    let set         (codec: Codec<'Encoding, 'a>) = (Ok << Set <-> Array.ofSeq)           >.> array codec
    let neset       (codec: Codec<'Encoding, 'a>) = (Set >> NonEmptySet.tryOfSet >> Option.toResultWith (DecodeError.Uncategorized "Set is empty") <-> Array.ofSeq) >.> array codec
    let resizeArray (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << ResizeArray <-> Array.ofSeq)
    let multiMap    (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.multiMap (Codec.upCast codec) |> Codec.downCast   : Codec<'Encoding, PropertyList<'a>>
    let map         (codec: Codec<'Encoding, 'a>) = (Ok << Map.ofSeq        << MultiMap.toSeq <-> (Map.toSeq        >> MultiMap.ofSeq)) >.> multiMap codec
    let dictionary  (codec: Codec<'Encoding, 'a>) = (Ok << Dictionary.ofSeq << MultiMap.toSeq <-> (Dictionary.toSeq >> MultiMap.ofSeq)) >.> multiMap codec
    let nemap       (codec: Codec<'Encoding, 'a>) = (MultiMap.toSeq >> Map.ofSeq >> NonEmptyMap.tryOfMap >> Option.toResultWith (DecodeError.Uncategorized "Map is empty") <-> (NonEmptyMap.toSeq >> MultiMap.ofSeq)) >.> multiMap codec
    let option   (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.option   (Codec.upCast codec) |> Codec.downCast  : Codec<'Encoding, option<'a>>
    let nullable (codec: Codec<'Encoding, 'a>) = (Ok << Option.toNullable <-> Option.ofNullable) >.> option codec  : Codec<'Encoding, Nullable<'a>>
    let result  (codec1: Codec<'Encoding, 'a>)  (codec2: Codec<'Encoding, 'b>) = instance<'Encoding>.result (Codec.upCast codec1) (Codec.upCast codec2) |> Codec.downCast : Codec<'Encoding, Result<'a,'b>>
    let choice  (codec1: Codec<'Encoding, 'a>)  (codec2: Codec<'Encoding, 'b>) = instance<'Encoding>.choice (Codec.upCast codec1) (Codec.upCast codec2) |> Codec.downCast : Codec<'Encoding, Choice<'a,'b>>
    let choice3 (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) = instance<'Encoding>.choice3 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) |> Codec.downCast : Codec<'Encoding, _>
    let tuple1  (codec1: Codec<'Encoding, 't1>) = instance<'Encoding>.tuple1 (Codec.upCast codec1) |> Codec.downCast : Codec<'Encoding, _>
    let tuple2  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) = instance<'Encoding>.tuple2 (Codec.upCast codec1) (Codec.upCast codec2) |> Codec.downCast : Codec<'Encoding, _>
    let tuple3  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) = instance<'Encoding>.tuple3 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) |> Codec.downCast : Codec<'Encoding, _>
    let tuple4  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) (codec4: Codec<'Encoding, 't4>) = instance<'Encoding>.tuple4 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) (Codec.upCast codec4) |> Codec.downCast : Codec<'Encoding, _>
    let tuple5  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) (codec4: Codec<'Encoding, 't4>) (codec5: Codec<'Encoding, 't5>) = instance<'Encoding>.tuple5 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) (Codec.upCast codec4) (Codec.upCast codec5) |> Codec.downCast : Codec<'Encoding, _>
    let tuple6  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) (codec4: Codec<'Encoding, 't4>) (codec5: Codec<'Encoding, 't5>) (codec6: Codec<'Encoding, 't6>) = instance<'Encoding>.tuple6 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) (Codec.upCast codec4) (Codec.upCast codec5) (Codec.upCast codec6) |> Codec.downCast : Codec<'Encoding, _>
    let tuple7  (codec1: Codec<'Encoding, 't1>) (codec2: Codec<'Encoding, 't2>) (codec3: Codec<'Encoding, 't3>) (codec4: Codec<'Encoding, 't4>) (codec5: Codec<'Encoding, 't5>) (codec6: Codec<'Encoding, 't6>) (codec7: Codec<'Encoding, 't7>) = instance<'Encoding>.tuple7 (Codec.upCast codec1) (Codec.upCast codec2) (Codec.upCast codec3) (Codec.upCast codec4) (Codec.upCast codec5) (Codec.upCast codec6) (Codec.upCast codec7) |> Codec.downCast : Codec<'Encoding, _>
    let base64Bytes<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = (Ok << Convert.FromBase64String <-> Convert.ToBase64String) >.> instance<'Encoding>.string |> Codec.downCast : Codec<'Encoding, _>

    let enum (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.enum (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, 'u>

    #if !FABLE_COMPILER
    let arraySegment (codec: Codec<'Encoding, 'a>) = (Ok << ArraySegment<_> << Seq.toArray <-> ArraySegment.toArray) >.> array codec
    #endif

    let gmap (keyCodec: Codec<'Encoding, 'a>) (valueCodec: Codec<'Encoding, 'b>) =
        let c = list (tuple2 keyCodec valueCodec)
        ((Ok << Map.ofList) <-> Map.toList) >.> c




type GetCodec =
    interface IDefault1

    static member GetCodec (_: bool          , _: GetCodec, _, _: 'Operation) = Codecs.boolean        : Codec<'Encoding, _>
    static member GetCodec (_: string        , _: GetCodec, _, _: 'Operation) = Codecs.string         : Codec<'Encoding, _>
    static member GetCodec (_: DateTime      , _: GetCodec, _, _: 'Operation) = Codecs.dateTime       : Codec<'Encoding, _>
    static member GetCodec (_: DateTimeOffset, _: GetCodec, _, _: 'Operation) = Codecs.dateTimeOffset : Codec<'Encoding, _>
    static member GetCodec (_: TimeSpan      , _: GetCodec, _, _: 'Operation) = Codecs.timeSpan       : Codec<'Encoding, _>
    static member GetCodec (_: decimal       , _: GetCodec, _, _: 'Operation) = Codecs.decimal        : Codec<'Encoding, _>
    static member GetCodec (_: Double        , _: GetCodec, _, _: 'Operation) = Codecs.float          : Codec<'Encoding, _>
    static member GetCodec (_: Single        , _: GetCodec, _, _: 'Operation) = Codecs.float32        : Codec<'Encoding, _>
    static member GetCodec (_: int           , _: GetCodec, _, _: 'Operation) = Codecs.int            : Codec<'Encoding, _>
    static member GetCodec (_: uint32        , _: GetCodec, _, _: 'Operation) = Codecs.uint32         : Codec<'Encoding, _>
    static member GetCodec (_: int64         , _: GetCodec, _, _: 'Operation) = Codecs.int64          : Codec<'Encoding, _>
    static member GetCodec (_: uint64        , _: GetCodec, _, _: 'Operation) = Codecs.uint64         : Codec<'Encoding, _>
    static member GetCodec (_: int16         , _: GetCodec, _, _: 'Operation) = Codecs.int16          : Codec<'Encoding, _>
    static member GetCodec (_: uint16        , _: GetCodec, _, _: 'Operation) = Codecs.uint16         : Codec<'Encoding, _>
    static member GetCodec (_: byte          , _: GetCodec, _, _: 'Operation) = Codecs.byte           : Codec<'Encoding, _>
    static member GetCodec (_: sbyte         , _: GetCodec, _, _: 'Operation) = Codecs.sbyte          : Codec<'Encoding, _>
    static member GetCodec (_: char          , _: GetCodec, _, _: 'Operation) = Codecs.char           : Codec<'Encoding, _>
    static member GetCodec (_: Guid          , _: GetCodec, _, _: 'Operation) = Codecs.guid           : Codec<'Encoding, _>
    static member GetCodec (()               , _: GetCodec, _, _: 'Operation) = Codecs.unit           : Codec<'Encoding, _>

    // Dummy overloads
    static member GetCodec (_: OpCodec , _: GetCodec, _, _: OpEncode) = failwithf "Fleece internal error: this code should be unreachable." : Codec<'Encoding, OpCodec>
    static member GetCodec (_: OpEncode, _: GetCodec, _, _: OpEncode) = failwithf "Fleece internal error: this code should be unreachable." : Codec<'Encoding, OpEncode>


    /// Invoker for Codec
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetCodec>, x)

    static member inline InvokeEx<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't, _: 'Class) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<'Class>, x)

type GetDec =
    inherit GetCodec
     
    /// Invoker for Codec, originated from a Decoder Invoker.
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetDec>, x)

type GetEnc =
    inherit GetCodec
    
    /// Invoker for Codec, originated from an Encoder Invoker.
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetEnc>, x)


type GetCodec with
    static member inline GetCodec (_: Tuple<'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Tuple<'a>> = Codecs.tuple1 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
    static member inline GetCodec (_: 'a Id1    when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _, _: 'Operation) = Ok (Id1<'a> Unchecked.defaultof<'a>), Map.empty


type GetCodec with
    static member inline GetCodec (_:'tuple when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, _> =
        let { Decoder = ofArray; Encoder = toArray } = Codecs.array (Ok <-> id)
        let c1 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t1>, c)
        let c2 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t2>, c)
        let c3 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t3>, c)
        let c4 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t4>, c)
        let c5 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t5>, c)
        let c6 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t6>, c)
        let c7 = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'t7>, c)
        let cr = GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'tr>, c)
        (fun x ->
            match ofArray x with
            | Ok a ->
                let (t1: 't1 ParseResult) = (Codec.decode c1) (a.[0])
                let (t2: 't2 ParseResult) = (Codec.decode c2) (a.[1])
                let (t3: 't3 ParseResult) = (Codec.decode c3) (a.[2])
                let (t4: 't4 ParseResult) = (Codec.decode c4) (a.[3])
                let (t5: 't5 ParseResult) = (Codec.decode c5) (a.[4])
                let (t6: 't6 ParseResult) = (Codec.decode c6) (a.[5])
                let (t7: 't7 ParseResult) = (Codec.decode c7) (a.[6])
                let (tr: 'tr ParseResult) = (Codec.decode cr) (toArray (a.[7..]))
                match tr with
                | Error (IndexOutOfRange (i, _)) -> Error (IndexOutOfRange (i + 8, x))
                | _ -> curryN (Tuple<_,_,_,_,_,_,_,_> >> retype : _ -> 'tuple) <!> t1 <*> t2 <*> t3 <*> t4 <*> t5 <*> t6 <*> t7 <*> tr
            | Error e -> Error e)
        <->
        fun (t: 'tuple) ->
            let t1 = (Codec.encode c1) (^tuple: (member Item1: 't1) t)
            let t2 = (Codec.encode c2) (^tuple: (member Item2: 't2) t)
            let t3 = (Codec.encode c3) (^tuple: (member Item3: 't3) t)
            let t4 = (Codec.encode c4) (^tuple: (member Item4: 't4) t)
            let t5 = (Codec.encode c5) (^tuple: (member Item5: 't5) t)
            let t6 = (Codec.encode c6) (^tuple: (member Item6: 't6) t)
            let t7 = (Codec.encode c7) (^tuple: (member Item7: 't7) t)
            let tr = (Codec.encode cr) (^tuple: (member Rest : 'tr) t) |> ofArray
            match tr with
            | Error _ -> failwith "Nested tuple didn't decompose as list, check your `list` and your `tupleX` encoder implementations."
            | Ok tr -> toArray ([|t1; t2; t3; t4; t5; t6; t7|] ++ tr)




type GetCodec with static member inline GetCodec (_: Result<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Result<'a,'b>> = Codecs.result (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c))
type GetCodec with static member inline GetCodec (_: Choice<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Choice<'a,'b>> = Codecs.choice (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c))
type GetCodec with static member inline GetCodec (_: Choice<'a, 'b, 'c> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Choice<'a,'b,'c>> = Codecs.choice3 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c))
type GetCodec with static member inline GetCodec (_: 'a option when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, option<'a>> = Codecs.option (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
type GetCodec with static member inline GetCodec (_: 'a Nullable when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Nullable<'a>> = Codecs.nullable (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
type GetCodec with static member inline GetCodec (_: NonEmptyList<'T> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, NonEmptyList<'T>> = Codecs.nelist (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'T>, c))


type GetCodec with
    static member inline GetCodec (_: 'a array  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, array<'a>> = Codecs.array  (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))

    #if !FABLE_COMPILER
    static member inline GetCodec (_: ArraySegment<'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, ArraySegment<'a>> = Codecs.arraySegment (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
    #endif

type GetCodec with static member inline GetCodec (_: list<'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, list<'a>>  = Codecs.list (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
type GetCodec with static member inline GetCodec (_: Set<'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Set<'a>>    = Codecs.set  (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
type GetCodec with static member inline GetCodec (_: NonEmptySet<'a>   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, NonEmptySet<'a>> = Codecs.neset (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
type GetCodec with static member inline GetCodec (_: Map<string, 'a>   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Map<string, 'a>> = Codecs.map   (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))

type GetCodec with static member inline GetCodec (_: PropertyList<'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, PropertyList<'a>> = Codecs.multiMap (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))

type GetCodec with
    static member inline GetCodec (_: NonEmptyMap<string, 'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding,NonEmptyMap<string, 'a>> = Codecs.nemap       (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
    static member inline GetCodec (_: Dictionary<string, 'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Dictionary<string, 'a>> = Codecs.dictionary  (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
    static member inline GetCodec (_: ResizeArray<'a>         when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, ResizeArray<'a>>        = Codecs.resizeArray (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c))
    static member inline GetCodec (_: 'a Id2   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _, _: 'Operation)  = (Ok (Id2<'a> Unchecked.defaultof<'a>) ), Map.empty

type GetCodec with static member inline GetCodec (_: 'a * 'b                          when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b                         > = Codecs.tuple2 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c))
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c                     when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c                    > = Codecs.tuple3 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c))
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd                when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd               > = Codecs.tuple4 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'d>, c))
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e           when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e          > = Codecs.tuple5 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'d>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'e>, c))
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f      when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f     > = Codecs.tuple6 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'d>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'e>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'f>, c))
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f * 'g> = Codecs.tuple7 (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'a>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'b>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'c>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'d>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'e>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'f>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'g>, c))

// requires F# 5 -->
type GetCodec with static member inline GetCodec (_: 't when 't : enum<_> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType, _: GetCodec, c, _: 'Operation) = Codecs.enum (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'u>, c))

type CodecCollection<'Encoding, 'Interface> () =
    static let mutable subtypes : Dictionary<Type, unit -> Codec<PropertyList<'Encoding>, 'Interface>> = new Dictionary<_,_> ()
    static member GetSubtypes   : Dictionary<Type, unit -> Codec<PropertyList<'Encoding>, 'Interface>> = subtypes
    static member AddSubtype ty x =
        match Dictionary.tryGetValue ty subtypes with
        | Some _ -> ()
        | None -> subtypes.Add (ty, x)



[<AutoOpen>]
module Functions =
    /// Creates a codec from/to 'Encoding from an Object-Codec.
    /// <param name="objCodec">A codec of MultiMap from/to a strong type.</param>
    /// <returns>A codec of IEncoding from/to a strong type.</returns>
    let ofObjCodec (objCodec: Codec<PropertyList<'Encoding>, 't>) : Codec<_, 't> = objCodec >.> Codecs.multiMap (Ok <-> id)



type GetCodec with
    [<CompilerMessage("No Encoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: #OvDecEncError, _: OpEncode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (En)"
 
    [<CompilerMessage("No Decoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: #OvDecEncError, _: OpDecode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (De)"

    [<CompilerMessage("No codec method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: #OvDecEncError, _: OpCodec) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (Co)"

    [<CompilerMessage("No Encoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: _, _: OpEncode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (En)"
 
    [<CompilerMessage("No Decoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: _, _: OpDecode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (De)"

    [<CompilerMessage("No codec method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: _, _: OpCodec) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (Co)"

type GetCodec with
    static member inline GetCodec (_: 'Base when 'Base :> ICodecInterface<'Base>, _: IDefault7, _, _: 'Operation) : Codec<'Encoding, 'Base> when 'Encoding :> IEncoding and 'Encoding : struct =
        let choice (codecs: seq<Codec<_, _, 't, 't>>) : Codec<PropertyList<'Encoding>, _> =

            let head, tail = Seq.head codecs, Seq.tail codecs
            let r = foldBack (<|>) tail head
            r
        (
            let codecs = CodecCollection<'Encoding, 'Base>.GetSubtypes
            match toList codecs with
            | [] -> failwithf "Unexpected error: codec list is empty for interface %A to Encoding %A." typeof<'Base> typeof<'Encoding>
            | _  -> (codecs |> Seq.map (fun (KeyValue(_, x)) -> x ()) |> choice) >.> Codecs.multiMap (Ok <-> id)
        )







type GetCodec with

    // Overload to "passthrough" an IEncoding
    static member GetCodec (_: 'Encoding when 'Encoding :> IEncoding and 'Encoding : struct, _: IDefault6, _, _: 'Operation) = Ok <-> id : Codec<'Encoding, 'Encoding>

    

    static member inline GetCodec (_: 'T, _: IDefault6, _, _: 'Operation) : Codec<'Encoding, 'T> = // when 'Encoding :> IEncoding and 'Encoding : struct =
        // let c = (^T : (static member Codec: Codec< PropertyList<'Encoding>, 'T>) ())
        // (c |> Codec.compose (GetCodec.Invoke<'Encoding, _> (Unchecked.defaultof<PropertyList<'Encoding>>, Unchecked.defaultof<'Encoding>)))
        let c : Codec< 'Encoding, 'T> = (^T : (static member Codec: Codec< 'Encoding, 'T>) ())
        c

    static member inline GetCodec (_: 'T, _: IDefault7, _, _: 'Operation) =
        let mutable r = Unchecked.defaultof<Codec< 'Encoding, 'T>>
        let _ = (^T : (static member Codec : byref<Codec< 'Encoding, 'T>> -> unit) &r)
        r

    // For backwards compatibility
    // [<Obsolete("This function resolves to a deprecated 'JsonObjCodec' method and it won't be supported in future versions of this library. Please rename it to 'Codec' or 'get_Codec ()' and convert the result by applying the 'ofObjCodec' function.")>]
    // But adding the warning changes overload resolution.
    static member inline GetCodec (_: 'T, _: IDefault8, _, _: 'Operation) : Codec<'Encoding, 'T> =
        let c : Codec<PropertyList<'Encoding>, 'T> = (^T : (static member JsonObjCodec: Codec<PropertyList<'Encoding>, 'T>) ())
        ofObjCodec c

    static member inline GetCodec (_: 'T, _: IDefaultA, _, _: 'Operation) : Codec<'Encoding, 'T> =
        let d = fun js -> (^T : (static member OfJson: 'Encoding -> ^T ParseResult) js) : ^T ParseResult
        let e: 'T -> 'Encoding = fun t -> (^T : (static member ToJson : ^T -> 'Encoding) t)
        d <-> e
    
    static member inline GetCodec (_: 'T, _: IDefault9, _, _: 'Operation) : Codec<'Encoding, 'T> =
        let d = fun js -> (^T : (static member OfJson: 'Encoding -> ^T ParseResult) js) : ^T ParseResult
        let e =
            fun t ->
                let mutable r = Unchecked.defaultof<'Encoding>
                let _ = (^T : (static member Encode : ^T * byref<'Encoding> -> unit) (t, &r))
                r
        d <-> e

    // Overload for Maps where the Key is not a string
    static member inline GetCodec (_: Map<'K, 'V>, _: IDefault5, c, _: 'Operation) : Codec<'Encoding, Map<'K, 'V>> =
        Codecs.gmap (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'K>, c)) (GetCodec.InvokeEx<'Encoding, 'Operation, _, _> (Unchecked.defaultof<'V>, c))


type GetEnc with
    static member inline GetCodec (_: 't, _: IDefault2, _, _: OpEncode) : Codec<'Encoding, ^t> =
        let e =
            fun t ->
                let mutable r = Unchecked.defaultof<'Encoding>
                let _ = (^t : (static member Encode : ^t * byref<'Encoding> -> unit) (t, &r))
                r
        decoderNotAvailable <-> e

type GetEnc with
    static member inline GetCodec (_: 't , _: IDefault1, _, _: OpEncode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let e: 't -> 'Encoding = fun t -> (^t : (static member ToJson : ^t -> 'Encoding) t)
        decoderNotAvailable <-> e

type GetDec with
    [<Obsolete("This function resolves to a deprecated 'OfJson' overload, returning a string as an error and it won't be supported in future versions of this library. Please update the 'OfJson' method, using the 'Fail' module to create a DecodeError.")>]
    static member inline GetCodec (_: 't , _: IDefault2, _, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> Result.bindError (Error << DecodeError.Uncategorized) (^t : (static member OfJson: 'Encoding -> Result< ^t, string>) js)
        d <-> encoderNotAvailable

type GetDec with
    static member inline GetCodec (_: 't , _: IDefault1, _, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> (^t : (static member OfJson: 'Encoding -> ^t ParseResult) js) : ^t ParseResult
        d <-> encoderNotAvailable


type CodecCache<'Operation, 'Encoding, 'T when 'Encoding :> IEncoding and 'Encoding : struct> () =
    static let mutable cachedCodec : option<Codec<'Encoding, 'T>> = None
    static member getCache () = cachedCodec
    static member Run<'Encoding, 'T> (f: unit -> Codec<'Encoding, 'T>) =
        if not Config.codecCacheEnabled then f ()
            else
            match cachedCodec with
            | Some c -> c
            | None   ->
                match CodecCache<OpDecode, 'Encoding, 'T>.getCache (), CodecCache<OpEncode, 'Encoding, 'T>.getCache () with
                | Some d, Some e -> d.Decoder <-> e.Encoder
                | _ ->
                    let c = f ()
                    cachedCodec <- Some c
                    c

    static member Run<'Operation, 'Encoding, 'T> (f: unit -> Codec<'Encoding, 'T>) =
        if not Config.codecCacheEnabled then f ()
        else
            match cachedCodec with
            | Some c -> c
            | None   ->
                match CodecCache<OpCodec, 'Encoding, 'T>.getCache () with
                | Some c -> c
                | _ ->
                    let c = f ()
                    cachedCodec <- Some c
                    c

[<AutoOpen>]
module Operators =

    // Creates a Codec from a pair of decoder and encoder functions, used mainly internally and in Encoding implementations.
    let (<->) decoder encoder : Codec<_,_,_,_> = { Decoder = decoder; Encoder = encoder }

    let (|Codec|) { Decoder = x; Encoder = y } = (x, y)

    let inline toEncoding< 'Encoding, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : 'Encoding =
        let codec = CodecCache<OpEncode, 'Encoding, 't>.Run<OpEncode, 'Encoding, 't> (fun () -> GetEnc.Invoke<'Encoding, OpEncode, _> x)
        (codec |> Codec.encode) x

    let inline ofEncoding (x: 'Encoding when 'Encoding :> IEncoding and 'Encoding : struct) : Result<'t, _> =
        let codec = CodecCache<OpDecode, 'Encoding, 't>.Run<OpDecode, 'Encoding, 't> (fun () -> GetDec.Invoke<'Encoding, OpDecode, _> Unchecked.defaultof<'t>)
        (codec |> Codec.decode) x
            



    let jreqWith (c: Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: PropertyList<_>) key =
            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key m
            | value:: _ -> decoder value
        {
            Decoder = fun (o: PropertyList<'Encoding>) -> getFromListWith (Codec.decode c) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, Codec.encode c x)] | _ -> zero
        }

    let jreqWithLazy (c: unit -> Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: PropertyList<_>) key =

            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key m
            | value:: _ -> decoder value
        {
            Decoder = fun (o: PropertyList<'Encoding>) -> getFromListWith (Codec.decode (c ())) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, Codec.encode (c ()) x)] | _ -> zero
        }

    /// Derive automatically a RawCodec, based on GetCodec / Codec static members
    let inline getCodec<'Encoding, .. when 'Encoding :> IEncoding and 'Encoding : struct> () : Codec<'Encoding, 't> =
        CodecCache.Run<'Encoding,'t> (fun () -> GetCodec.Invoke<'Encoding, OpCodec, 't> Unchecked.defaultof<'t>)



    /// <summary>Derives a concrete field object codec for a required field.</summary>
    /// <param name="name">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jreq (name: string) (getter: 'T -> 'param option) : Codec<PropertyList<'Encoding>, PropertyList<'Encoding>, 'param, 'T> = jreqWithLazy (getCodec<'Encoding, 'param>) name getter

    /// <summary>Same as jopt but using an explicit codec.</summary>
    let joptWith c (prop: string) (getter: 'T -> 'Value option) =
        let getFromListOptWith decoder (m: PropertyList<_>) key =
            match m.[key] with
            | []        -> Ok None
            | value:: _ -> decoder value |> Result.map Some
        {
            Decoder = fun (o: PropertyList<'S>) -> getFromListOptWith (Codec.decode c) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, Codec.encode c x)] | _ -> zero
        }

    /// Derives a concrete field codec for an optional field
    let inline jopt prop (getter: 'T -> 'param option) : Codec<PropertyList<'Encoding>, PropertyList<'Encoding>, 'param option, 'T> = joptWith (getCodec<'Encoding, 'param> ()) prop getter



    let jobj (x: list<string * 'Encoding>) : 'Encoding =
        let (Codec (_, enc)) = Codecs.multiMap (Ok <-> id)
        multiMap (x |> Seq.map System.Collections.Generic.KeyValuePair)
        |> enc

    let JNull<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> : 'Encoding = (Codecs.option Codecs.unit |> Codec.encode) None
    let JBool   x = (Codecs.boolean |> Codec.encode) x
    let JNumber x = (Codecs.decimal |> Codec.encode) x
    let JString x = (Codecs.string  |> Codec.encode) x
    let JArray (x: IReadOnlyList<'Encoding>) = (Codecs.array (Ok <-> id) |> Codec.encode) (toArray x)
    let JObject x = (Codecs.multiMap (Ok <-> id) |> Codec.encode) x
    
    let (|JNull|_|)   (x: 'Encoding) = match (Codecs.option (Ok <-> id) |> Codec.decode) x with | Ok None -> Some () | _ -> None    
    let (|JBool|_|)   (x: 'Encoding) = (Codecs.boolean |> Codec.decode) x |> Option.ofResult
    let (|JNumber|_|) (x: 'Encoding) = (Codecs.decimal |> Codec.decode) x |> Option.ofResult
    let (|JString|_|) (x: 'Encoding) = (Codecs.string  |> Codec.decode) x |> Option.ofResult
    let (|JArray|_|)  (x: 'Encoding) = (Codecs.array    (Ok <-> id) |> Codec.decode) x |> Option.ofResult |> Option.map IReadOnlyList.ofArray
    let (|JObject|_|) (x: 'Encoding) = (Codecs.multiMap (Ok <-> id) |> Codec.decode) x |> Option.ofResult

    
    /// Gets a value from an Encoding object.
    let jgetWith decoder (o: PropertyList<'Encoding>) key =
        match o.[key] with
        | value::_ -> decoder value
        | _ -> Decode.Fail.propertyNotFound key o

    /// Tries to get a value from an Encoding object.
    /// Returns None if key is not present in the object.
    let jgetOptWith decoder (o: PropertyList<'Encoding>) key =
        match o.[key] with
        | JNull _::_ -> Ok None
        | value  ::_ -> decoder value |> Result.map Some
        | _ -> Ok None

    /// Gets a value from an Encoding object.
    let inline jget (o: PropertyList<' Encoding>) key = jgetWith ofEncoding o key

    /// Tries to get a value from an Encoding object.
    /// Returns None if key is not present in the object.
    let inline jgetOpt (o: PropertyList<' Encoding>) key = jgetOptWith ofEncoding o key

    /// Gets a value from an Encoding object.
    let inline (.@) o key = jget o key

    /// Tries to get a value from an Encoding object.
    /// Returns None if key is not present in the object.
    let inline (.@?) o key = jgetOpt o key

    /// Creates a new Encoding key-value pair for an Encoding object.
    let inline jpair (key: string) (value: 'T) = map toEncoding (key, value)

    /// Creates a new Encoding key-value pair for an Encoding object.
    let inline (.=) key value = jpair key value
    
    
[<AutoOpen>]
module CodecInterfaceExtensions =
    type ICodecInterface<'Base> with
        /// This is the entry point to register codecs for interface implementations.
        static member RegisterCodec<'Encoding, 'Type> (codec: unit -> Codec<PropertyList<'Encoding>, 'Type>) =
            let codec () =
                let objCodec = codec ()
                let (d, e) = objCodec.Decoder, objCodec.Encoder
                let nd = d >> Result.map (fun (x: 'Type) -> retype x : 'Base)
                let ne =
                    fun (x: 'Base) ->
                        match box x with
                            | :? 'Type as t -> e t
                            | _ -> zero
                { Decoder = nd; Encoder = ne }
            codec |> CodecCollection<'Encoding, 'Base>.AddSubtype typeof<'Type>

[<AutoOpen>]
module ComputationExpressions =
    type CodecApplicativeBuilder () =

        let privReturn f = ({ Decoder = (fun _ -> Ok f); Encoder = zero }) : Codec<PropertyList<'S>, PropertyList<'S>,_,_>
        let privlift2 (f: 'x ->'y ->'r) (x: Codec<PropertyList<'S>, PropertyList<'S>,'x,'T>) (y:  Codec<PropertyList<'S>, PropertyList<'S>,'y,'T>) : Codec<PropertyList<'S>, PropertyList<'S>,'r,'T> =
                {
                    Decoder = fun s -> lift2 f (x.Decoder s) (y.Decoder s)
                    Encoder = x.Encoder ++ y.Encoder
                }
        let privlift3 (f: 'x -> 'y -> 'z -> 'r) (x: Codec<PropertyList<'S>, PropertyList<'S>,'x,'T>) (y: Codec<PropertyList<'S>, PropertyList<'S>,'y,'T>) (z: Codec<PropertyList<'S>, PropertyList<'S>,'z,'T>) : Codec<PropertyList<'S>, PropertyList<'S>,'r,'T> =
                {
                    Decoder = fun s -> lift3 f (x.Decoder s) (y.Decoder s) (z.Decoder s)
                    Encoder = x.Encoder ++ y.Encoder ++ z.Encoder
                }

        member _.Delay x = x ()
        member _.ReturnFrom expr = expr
        member _.Return x = privReturn x
        member _.Yield  x : Codec<PropertyList<'r>, 't> = x
        member _.MergeSources  (t1, t2)     = privlift2 tuple2 t1 t2
        member _.MergeSources3 (t1, t2, t3) = privlift3 tuple3 t1 t2 t3
        member _.BindReturn (x: Codec<PropertyList<'r>, PropertyList<'r>,_,_>, f) = f <!> x
        member _.Run x : Codec<PropertyList<_>,'t> = x
        member _.Combine (x: Codec<PropertyList<'S>, 't>, y: Codec<PropertyList<'S>, 't>) = x <|> y : Codec<PropertyList<'S>, 't>

    /// Codec Applicative Computation Expression.
    let codec= CodecApplicativeBuilder ()


module Lens =
    open FSharpPlus.Lens
    let inline _JString x = (prism' JString <| function JString s -> Some s | _ -> None) x
    let inline _JObject x = (prism' JObject <| function JObject s -> Some s | _ -> None) x
    let inline _JArray  x = (prism' JArray  <| function JArray  s -> Some s | _ -> None) x
    let inline _JBool   x = (prism' JBool   <| function JBool   s -> Some s | _ -> None) x
    let inline _JNumber x = (prism' JNumber <| fun v -> match Operators.ofEncoding v : decimal ParseResult with Ok s -> Some s | _ -> None) x
    let inline _JNull   x = prism' (konst JNull) (function JNull -> Some () | _ -> None) x

    /// Like '_jnth', but for 'Object' with Text indices.
    let inline _jkey i =
        let inline dkey i f t = map (fun x -> MultiMap.add i x t) (f (t.[i] |> function [] -> JNull | x::_ -> x))
        _JObject << dkey i

    let inline _jnth i =
        let inline dnth i f t = map (fun x -> t |> IReadOnlyList.trySetItem i x |> Option.defaultValue t) (f (IReadOnlyList.tryItem i t |> Option.defaultValue JNull))
        _JArray << dnth i

    // Reimport some basic Lens operations from F#+

    let setl optic value   (source: 's) : 't = setl optic value source
    let over optic updater (source: 's) : 't = over optic updater source
    let preview (optic: ('a -> Const<_,'b>) -> _ -> Const<_,'t>) (source: 's) : 'a option = preview optic source