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

type IDefault8 = interface inherit OvDecEncError end
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
type IInterfaceCodec<'Base> = interface end

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

open Helpers


/// Encodes a value of a generic type 't into a value of raw type 'S.
type Encoder<'S, 't> = 't -> 'S


/// An alias for a MultimMap with string keys
type MultiObj<'t> = MultiMap<string, 't>

/// A decoder from raw type 'S1 and encoder to raw type 'S2 for string types 't1 and 't2.
type Codec<'S1, 'S2, 't1, 't2> = { Decoder : Decoder<'S1, 't1>; Encoder : Encoder<'S2, 't2> } with
    static member inline Return f = { Decoder = (fun _ -> Ok f); Encoder = zero }


/// A codec for raw type 'S decoding to strong type 't1 and encoding to strong type 't2.
and SplitCodec<'S, 't1, 't2> = Codec<'S, 'S, 't1, 't2>

/// A decoder from raw type 'S1 and encoder to raw type 'S2 for type 't.
and Codec<'S1, 'S2, 't> = Codec<'S1, 'S2, 't, 't>

/// A codec for raw type 'S to strong type 't.
and Codec<'S, 't> = Codec<'S, 'S, 't>

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
    abstract nullable       : Codec<IEncoding, 't>  -> Codec<IEncoding, Nullable<'t>>
    abstract array          : Codec<IEncoding, 't>  -> Codec<IEncoding, 't []>
    abstract multiMap       : Codec<IEncoding, 't>  -> Codec<IEncoding, MultiObj<'t>>
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
    | PropertyNotFound of string * MultiObj<IEncoding>
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



module Operators =
    // Creates a Codec from a pair of decoder and encoder functions, used mainly internally and in Encoding implementations.
    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    let (<->) decoder encoder : Codec<_,_> = { Decoder = decoder ; Encoder = encoder }

    let (|Codec|) { Decoder = x ; Encoder = y } = (x, y)
    let dec (Codec (d, _)) = d
    let enc (Codec (_, e)) = e

open Operators

/// Functions operating on Codecs
module Codec =

    let decode (Codec (d, _)) = d
    let encode (Codec (_, e)) = e

    /// Turns a Codec into another Codec, by mapping it over an isomorphism.
    let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) c =
        let (Codec (r, w)) = c
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
        let (Codec (dec1, enc1)) = codec1
        let (Codec (dec2, enc2)) = codec2
        (dec1 >> (=<<) dec2) <-> (enc1 << enc2)

    /// Maps a function over the decoder.
    let map (f: 't1 -> 'u1) (field: Codec<MultiObj<'S>, MultiObj<'S>, 't1, 't2>) =
        {
            Decoder = fun x ->
                match field.Decoder x with
                | Error e -> Error e
                | Ok a    -> Ok (f a)

            Encoder = field.Encoder
        }

    let downCast<'t,'S when 'S :> IEncoding> (x: Codec<IEncoding, IEncoding, 't,'t> ) : Codec<'S, 'S, 't, 't>=
        {
            Decoder = fun (p: 'S) -> (x).Decoder (p :> IEncoding)
            Encoder = fun (p: 't) -> x.Encoder p :?> 'S
        }

    let upCast<'t,'S when 'S :> IEncoding> (x: Codec<'S, 'S, 't, 't>) : Codec<IEncoding, IEncoding, 't,'t> =
        {
            Decoder = fun (p: IEncoding) -> (x).Decoder (p :?> 'S)
            Encoder = fun (p: 't) -> x.Encoder p :> IEncoding
        }

    
    [<Obsolete("This function is no longer needed. You can safely remove it.")>]
    let ofConcrete x = id x

    [<Obsolete("This function is no longer needed. You can safely remove it.")>]
    let toConcrete x = id x


type Codec<'S1, 'S2, 't1, 't2> with

    static member (<*>) (remainderFields: Codec<MultiObj<'S>, MultiObj<'S>, 'f ->'r, 'T>, currentField: Codec<MultiObj<'S>, MultiObj<'S>, 'f, 'T>) =
        {
            Decoder = fun x ->
                match remainderFields.Decoder x, lazy (currentField.Decoder x) with
                | Error e, _ | _, Lazy (Error e) -> Error e
                | Ok a   , Lazy (Ok b)           -> Ok (a b)

            Encoder = fun t -> remainderFields.Encoder t ++ currentField.Encoder t
        }

    /// Apply two codecs in such a way that the field values are ignored when decoding.
    static member ( *>) (f: Codec<MultiObj<'S>, MultiObj<'S>, 't, 'u>, x) = f *> x : Codec<MultiObj<'S>, 'u>

    /// Apply two codecs in such a way that the field values are ignored when decoding.
    static member (<* )  (x, f: Codec<MultiObj<'S>, MultiObj<'S>, 't, 'u>) = x <* f : Codec<MultiObj<'S>, 'u>

    static member (<!>) (f, field: Codec<MultiObj<'S>, MultiObj<'S>, 'f, 'T>) = Codec.map f field
    static member Map   (field: Codec<MultiObj<'S>, MultiObj<'S>, 'f, 'T>, f) = Codec.map f field

    static member (<|>) (source: Codec<MultiObj<'S>, MultiObj<'S>, 'f, 'T>, alternative: Codec<MultiObj<'S>, MultiObj<'S>, 'f, 'T>) =
        {
            Decoder = fun r ->
                match source.Decoder r, lazy (alternative.Decoder r) with
                | Ok x, _ -> Ok x
                | Error x, Lazy (Error y) -> Error (x ++ y)
                | _, Lazy d -> d

            Encoder = fun t -> source.Encoder t ++ alternative.Encoder t
        }

    static member Lift2 (f: 'x ->'y ->'r, x: Codec<MultiObj<'S>, MultiObj<'S>,'x,'T>, y: Codec<MultiObj<'S>, MultiObj<'S>,'y,'T>) : Codec<MultiObj<'S>, MultiObj<'S>,'r,'T> =
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
        let inline count e a = Error (IndexOutOfRange (e, a))
        let invalidValue v o : Result<'t, _> = Error (InvalidValue (typeof<'t>, v, o))
        let propertyNotFound p o = Error (PropertyNotFound (p, o))
        let parseError s v : Result<'t, _> = Error (ParseError (typeof<'t>, s, v))






module Codecs =

    let private instance<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = Unchecked.defaultof<'Encoding>

    let unit<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.unit           |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let boolean<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.boolean        |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let guid<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.guid           |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let char<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.char           |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let byte<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>           = instance<'Encoding>.byte           |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let sbyte<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.sbyte          |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let uint16<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint16         |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let uint32<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint32         |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let uint64<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.uint64         |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let int16<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.int16          |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let int<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>            = instance<'Encoding>.int            |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let int64<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.int64          |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let decimal<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.decimal        |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let float32<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>        = instance<'Encoding>.float32        |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let float<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>          = instance<'Encoding>.float          |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let string<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>         = instance<'Encoding>.string         |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let dateTime<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>       = instance<'Encoding>.dateTime       |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let dateTimeOffset<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = instance<'Encoding>.dateTimeOffset |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let timeSpan<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct>       = instance<'Encoding>.timeSpan       |> Codec.downCast : Codec<'Encoding, 'Encoding, _>
    let array       (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.array (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, array<'a>>
    let list        (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << Array.toList <-> Array.ofList)
    let resizeArray (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << ResizeArray <-> Array.ofSeq)
    let set         (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << Set <-> Array.ofSeq)
    let multiMap    (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.multiMap (Codec.upCast codec) |> Codec.downCast   : Codec<'Encoding, MultiObj<'a>>
    let map         (codec: Codec<'Encoding, 'a>) = Codec.compose (multiMap codec) (Ok << Map.ofSeq << MultiMap.toSeq <-> (Map.toSeq >> MultiMap.ofSeq))
    let dictionary  (codec: Codec<'Encoding, 'a>) = Codec.compose (multiMap codec) (Ok << Dictionary.ofSeq << MultiMap.toSeq <-> (Dictionary.toSeq >> MultiMap.ofSeq))
    let option   (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.option   (Codec.upCast codec) |> Codec.downCast  : Codec<'Encoding, option<'a>>
    let nullable (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.nullable (Codec.upCast codec) |> Codec.downCast  : Codec<'Encoding, Nullable<'a>>
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
    let base64Bytes<'Encoding when 'Encoding :> IEncoding and 'Encoding : struct> = (Codec.compose instance<'Encoding>.string (Ok << Convert.FromBase64String <-> Convert.ToBase64String)) |> Codec.downCast : Codec<'Encoding, 'Encoding, _>

    let enum (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.enum (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, 'u>

    #if !FABLE_COMPILER
    let arraySegment (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << ArraySegment<_> << Seq.toArray <-> ArraySegment.toArray)
    #endif

    let gmap (keyCodec: Codec<'Encoding, 'a>) (valueCodec: Codec<'Encoding, 'b>) =
        let c = list (tuple2 keyCodec valueCodec)
        ((Ok << Map.ofList) <-> Map.toList) |> Codec.compose c




type GetCodec =
    interface IDefault1

    static member GetCodec (_: bool          , _: GetCodec, _: 'Operation) = Codecs.boolean        : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: string        , _: GetCodec, _: 'Operation) = Codecs.string         : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: DateTime      , _: GetCodec, _: 'Operation) = Codecs.dateTime       : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: DateTimeOffset, _: GetCodec, _: 'Operation) = Codecs.dateTimeOffset : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: TimeSpan      , _: GetCodec, _: 'Operation) = Codecs.timeSpan       : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: decimal       , _: GetCodec, _: 'Operation) = Codecs.decimal        : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: Double        , _: GetCodec, _: 'Operation) = Codecs.float          : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: Single        , _: GetCodec, _: 'Operation) = Codecs.float32        : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: int           , _: GetCodec, _: 'Operation) = Codecs.int            : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: uint32        , _: GetCodec, _: 'Operation) = Codecs.uint32         : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: int64         , _: GetCodec, _: 'Operation) = Codecs.int64          : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: uint64        , _: GetCodec, _: 'Operation) = Codecs.uint64         : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: int16         , _: GetCodec, _: 'Operation) = Codecs.int16          : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: uint16        , _: GetCodec, _: 'Operation) = Codecs.uint16         : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: byte          , _: GetCodec, _: 'Operation) = Codecs.byte           : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: sbyte         , _: GetCodec, _: 'Operation) = Codecs.sbyte          : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: char          , _: GetCodec, _: 'Operation) = Codecs.char           : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (_: Guid          , _: GetCodec, _: 'Operation) = Codecs.guid           : Codec<'Encoding, 'Encoding, _>
    static member GetCodec (()               , _: GetCodec, _: 'Operation) = Codecs.unit           : Codec<'Encoding, 'Encoding, _>

    // Dummy overloads
    static member GetCodec (_: OpCodec  , _: GetCodec, _: OpEncode) =  Unchecked.defaultof<_>      : Codec<'Encoding, 'Encoding, OpCodec>
    static member GetCodec (_: OpEncode , _: GetCodec, _: OpEncode) =  Unchecked.defaultof<_>      : Codec<'Encoding, 'Encoding, OpEncode>


    /// Invoker for Codec
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a * _ -> Codec<'Encoding, ^t>) b, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetCodec>, x)

type GetDec =
    inherit GetCodec
     
    /// Invoker for Codec, originated from a Decoder Invoker.
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a * _ -> Codec<'Encoding, ^t>) b, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetDec>, x)

type GetEnc =
    inherit GetCodec
    
    /// Invoker for Codec, originated from an Encoder Invoker.
    static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : Codec<'Encoding, ^t> =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a * _ -> Codec<'Encoding, ^t>) b, a, Unchecked.defaultof<'Operation>)
        call (Unchecked.defaultof<GetEnc>, x)


type GetCodec with
    static member inline GetCodec (_: Tuple<'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Tuple<'a>> = Codecs.tuple1 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
    static member inline GetCodec (_: 'a Id1    when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) = Ok (Id1<'a> Unchecked.defaultof<'a>), Map.empty


type GetCodec with
    static member inline GetCodec (_:'tuple when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, _> =
        let (Codec (ofArray, toArray)) = Codecs.array (Ok <-> id)
        let c1 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t1>
        let c2 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t2>
        let c3 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t3>
        let c4 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t4>
        let c5 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t5>
        let c6 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t6>
        let c7 = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t7>
        let cr = GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'tr>
        (fun x ->
            match ofArray x with
            | Ok a ->
                let (t1: 't1 ParseResult) = (dec c1) (a.[0])
                let (t2: 't2 ParseResult) = (dec c2) (a.[1])
                let (t3: 't3 ParseResult) = (dec c3) (a.[2])
                let (t4: 't4 ParseResult) = (dec c4) (a.[3])
                let (t5: 't5 ParseResult) = (dec c5) (a.[4])
                let (t6: 't6 ParseResult) = (dec c6) (a.[5])
                let (t7: 't7 ParseResult) = (dec c7) (a.[6])
                let (tr: 'tr ParseResult) = (dec cr) (toArray (a.[7..]))
                match tr with
                | Error (IndexOutOfRange (i, _)) -> Error (IndexOutOfRange (i + 8, x))
                | _ -> curryN (Tuple<_,_,_,_,_,_,_,_> >> retype : _ -> 'tuple) <!> t1 <*> t2 <*> t3 <*> t4 <*> t5 <*> t6 <*> t7 <*> tr
            | Error e -> Error e)
        <->
        fun (t: 'tuple) ->
            let t1 = (enc c1) (^tuple: (member Item1: 't1) t)
            let t2 = (enc c2) (^tuple: (member Item2: 't2) t)
            let t3 = (enc c3) (^tuple: (member Item3: 't3) t)
            let t4 = (enc c4) (^tuple: (member Item4: 't4) t)
            let t5 = (enc c5) (^tuple: (member Item5: 't5) t)
            let t6 = (enc c6) (^tuple: (member Item6: 't6) t)
            let t7 = (enc c7) (^tuple: (member Item7: 't7) t)
            let tr = (enc cr) (^tuple: (member Rest : 'tr) t) |> ofArray
            match tr with
            | Error _ -> failwith "Nested tuple didn't decompose as list, check your `list` and your `tupleX` encoder implementations."
            | Ok tr -> toArray ([|t1; t2; t3; t4; t5; t6; t7|] ++ tr)




type GetCodec with static member inline GetCodec (_: Result<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Result<'a,'b>> = Codecs.result (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)
type GetCodec with static member inline GetCodec (_: Choice<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Choice<'a,'b>> = Codecs.choice (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)
type GetCodec with static member inline GetCodec (_: Choice<'a, 'b, 'c> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Choice<'a,'b,'c>> = Codecs.choice3 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>)
type GetCodec with static member inline GetCodec (_: 'a option when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, option<'a>> = Codecs.option (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
type GetCodec with static member inline GetCodec (_: 'a Nullable when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Nullable<'a>> = Codecs.nullable (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)


type GetCodec with
    static member inline GetCodec (_: 'a array  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, array<'a>>  = Codecs.array  (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)

    #if !FABLE_COMPILER
    static member inline GetCodec (_: ArraySegment<'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding,ArraySegment<'a>> = Codecs.arraySegment (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
    #endif

type GetCodec with static member inline GetCodec (_: list<'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding,list<'a>>    = Codecs.list   (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
type GetCodec with static member inline GetCodec (_: Set<'a>   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding,Set<'a>>    = Codecs.set    (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
type GetCodec with static member inline GetCodec (_: Map<string, 'a>   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Map<string, 'a>>    = Codecs.map (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)

type GetCodec with static member inline GetCodec (_: MultiObj<'a> when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, MultiObj<'a>> = Codecs.multiMap (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)

type GetCodec with
    static member inline GetCodec (_: Dictionary<string, 'a>   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, Dictionary<string, 'a>>    = Codecs.dictionary (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
    static member inline GetCodec (_: ResizeArray<'a>  when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding,ResizeArray<'a>> = Codecs.resizeArray   (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
    static member inline GetCodec (_: 'a Id2   when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation)  = (Ok (Id2<'a> Unchecked.defaultof<'a>) ), Map.empty

type GetCodec with static member inline GetCodec (_: 'a * 'b                          when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b                         > = Codecs.tuple2 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c                     when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c                    > = Codecs.tuple3 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>)
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd                when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd               > = Codecs.tuple4 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>)
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e           when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e          > = Codecs.tuple5 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>)
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f      when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f     > = Codecs.tuple6 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'f>)
type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g when 'Encoding :> IEncoding and 'Encoding : struct, _: GetCodec, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f * 'g> = Codecs.tuple7 (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'f>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'g>)

// requires F# 5 -->
type GetCodec with static member inline GetCodec (_: 't when 't : enum<_> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType, _: GetCodec, _: 'Operation) = Codecs.enum (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'u>)

type CodecCollection<'Encoding, 'Interface> () =
    static let mutable subtypes : Dictionary<Type, unit -> Codec<MultiObj<'Encoding>, 'Interface>> = new Dictionary<_,_> ()
    static member GetSubtypes   : Dictionary<Type, unit -> Codec<MultiObj<'Encoding>, 'Interface>> = subtypes
    static member AddSubtype ty x =
        match Dictionary.tryGetValue ty subtypes with
        | Some _ -> ()
        | None -> subtypes.Add (ty, x)



[<AutoOpen>]
module Functions =
    /// Creates a codec from/to IEncoding from an Object-Codec.
    /// <param name="objCodec">A codec of MultiMap from/to a strong type.</param>
    /// <returns>A codec of IEncoding from/to a strong type.</returns>
    let ofObjCodec (objCodec: Codec<MultiObj<'Encoding>, 't>) : Codec<_, 't> = (objCodec |> Codec.compose (Codecs.multiMap ((Ok <-> id))))



type GetCodec with
    [<CompilerMessage("No Encoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: OpEncode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (En)"
     
    [<CompilerMessage("No Decoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: OpDecode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (De)"

    [<CompilerMessage("No codec method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : not struct, _: OvDecEncError, _: OpCodec) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (Co)"

    [<CompilerMessage("No Encoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: OpEncode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (En)"
 
    [<CompilerMessage("No Decoder method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: OpDecode) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (De)"

    [<CompilerMessage("No codec method found.", 10708, IsError = true)>]
    static member inline GetCodec (_: 't when 't : struct, _: OvCodecError, _: OpCodec) : Codec<'Encoding, ^t> when 'Encoding :> IEncoding and 'Encoding : struct = failwith "Unreachable (Co)"

type GetCodec with
    static member inline GetCodec (_: 'Base when 'Base :> IInterfaceCodec<'Base>, _: IDefault7, _: 'Operation) : Codec<'Encoding, 'Base> when 'Encoding :> IEncoding and 'Encoding : struct =
        let choice (codecs: seq<Codec<_, _, 't, 't>>) : Codec<MultiObj<'Encoding>, _> =

            let head, tail = Seq.head codecs, Seq.tail codecs
            let r = foldBack (<|>) tail head
            r
        (
            let codecs = CodecCollection<'Encoding, 'Base>.GetSubtypes
            match toList codecs with
            | [] -> failwithf "Unexpected error: codec list is empty for interface %A to Encoding %A." typeof<'Base> typeof<'Encoding>
            | _  -> (codecs |> Seq.map (fun (KeyValue(_, x)) -> x ()) |> choice) |> Codec.compose (Codecs.multiMap (Ok <-> id))
        )







type GetCodec with

    // Overload to "passthrough" an IEncoding
    static member GetCodec (_: 'Encoding when 'Encoding :> IEncoding and 'Encoding : struct, _: IDefault6, _: 'Operation) = Ok <-> id : Codec<'Encoding, 'Encoding>

    static member inline GetCodec (_: 'T, _: IDefault6, _: 'Operation) : Codec<'Encoding, 'T> = // when 'Encoding :> IEncoding and 'Encoding : struct =
        // let c = (^T : (static member Codec: Codec< MultiObj<'Encoding>, 'T>) ())
        // (c |> Codec.compose (GetCodec.Invoke<'Encoding, _> (Unchecked.defaultof<MultiObj<'Encoding>>, Unchecked.defaultof<'Encoding>)))
        let c : Codec< 'Encoding, 'T> = (^T : (static member Codec: Codec< 'Encoding, 'T>) ())
        c

    // For backwards compatibility
    // [<Obsolete("This function resolves to a deprecated 'JsonObjCodec' method and it won't be supported in future versions of this library. Please rename it to 'Codec' or 'get_Codec ()' and convert the result by applying the 'ofObjCodec' function.")>]
    // But adding the warning changes overload resolution.
    static member inline GetCodec (_: 'T, _: IDefault8, _: 'Operation) : Codec<'Encoding, 'T> =
        let c : Codec<MultiObj<'Encoding>, 'T> = (^T : (static member JsonObjCodec: Codec<MultiObj<'Encoding>, 'T>) ())
        ofObjCodec c


    // Overload for Maps where the Key is not a string
    static member inline GetCodec (_: Map<'K,'V>, _: IDefault5, _: 'Operation) : Codec<'Encoding, Map<'K,'V>> =
        Codecs.gmap (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'K>) (GetCodec.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>)



    static member inline GetCodec (_: 't , _: IDefault4, _: OpEncode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let e: 't -> 'Encoding = fun t -> (^t : (static member ToJson : ^t -> 'Encoding) t)
        Unchecked.defaultof<_> <-> e

    static member inline GetCodec (_: 't , _: IDefault4, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> (^t : (static member OfJson: 'Encoding -> ^t ParseResult) js) : ^t ParseResult
        d <-> Unchecked.defaultof<_>

    [<Obsolete("This function resolves to a deprecated 'OfJson' overload, returning a string as an error and it won't be supported in future versions of this library. Please update the 'OfJson' method, using the 'Fail' module to create a DecodeError.")>]
    static member inline GetCodec (_: 't , _: IDefault3, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> Result.bindError (Error << DecodeError.Uncategorized) (^t : (static member OfJson: 'Encoding -> Result< ^t, string>) js)
        d <-> Unchecked.defaultof<_>


type GetEnc with
    static member inline GetCodec (_: 't , _: IDefault2, _: OpEncode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let e: 't -> 'Encoding = fun t -> (^t : (static member ToJson : ^t -> 'Encoding) t)
        Unchecked.defaultof<_> <-> e

type GetDec with
    static member inline GetCodec (_: 't , _: IDefault2, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> (^t : (static member OfJson: 'Encoding -> ^t ParseResult) js) : ^t ParseResult
        d <-> Unchecked.defaultof<_>

type GetDec with
    [<Obsolete("This function resolves to a deprecated 'OfJson' overload, returning a string as an error and it won't be supported in future versions of this library. Please update the 'OfJson' method, using the 'Fail' module to create a DecodeError.")>]
    static member inline GetCodec (_: 't , _: IDefault1, _: OpDecode) : Codec<'Encoding, ^t> (* when 'Encoding :> IEncoding and 'Encoding : struct *) =
        let d = fun js -> Result.bindError (Error << DecodeError.Uncategorized) (^t : (static member OfJson: 'Encoding -> Result< ^t, string>) js)
        d <-> Unchecked.defaultof<_>


type CodecCache<'Encoding, 'T when 'Encoding :> IEncoding and 'Encoding : struct> () =
    static let mutable cachedCodec : option<Codec<'Encoding, 'T>> = None
    static member Run<'Encoding,'T> (f: unit -> Codec<'Encoding, 'T>) =
        match cachedCodec with
        | Some c -> c
        | None   ->
            let c = f ()
            cachedCodec <- Some c
            c

[<AutoOpen>]
module MainFunctions =

    let inline toEncoding< 'Encoding, .. when 'Encoding :> IEncoding and 'Encoding : struct> (x: 't) : 'Encoding =
        // let codec = CodecCache.Run<'Encoding,'t> (fun () -> GetCodec.InvokeEnc<'Encoding, _> x)
        // (codec |> enc) x
        // GetCodec.InvokeDec.Invoke<'Encoding, _> Unchecked.defaultof<'t> x
        (GetEnc.Invoke<'Encoding, OpEncode, _> x |> enc) x

    let inline ofEncoding (x: 'Encoding when 'Encoding :> IEncoding and 'Encoding : struct) : Result<'t, _> =
        // let codec = CodecCache.Run<'Encoding,'t> (fun () -> GetCodec.InvokeDec<'Encoding, _> Unchecked.defaultof<'t>)
        // (codec |> dec) x
        // GetDecoder.Invoke<'Encoding, _> Unchecked.defaultof<'t> x
        (GetDec.Invoke<'Encoding, OpDecode, _> Unchecked.defaultof<'t> |> dec) x



    let reqWith (c: Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: MultiObj<_>) key =
            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key (m |> MultiMap.mapValues (fun x -> x :> IEncoding))
            | value:: _ -> decoder value
        {
            Decoder = fun (o: MultiObj<'Encoding>) -> getFromListWith (dec (c)) o prop
            Encoder = fun x -> (match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, (enc (c)) x)] | _ -> zero)
        }

    let reqWithLazy (c: unit -> Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: MultiObj<_>) key =

            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key (m |> MultiMap.mapValues (fun x -> x :> IEncoding))
            | value:: _ -> decoder value
        {
            Decoder = fun (o: MultiObj<'Encoding>) -> getFromListWith (dec (c ())) o prop
            Encoder = fun x -> (match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, (enc (c ())) x)] | _ -> zero)
        }

    /// Derive automatically a RawCodec, based on GetCodec / Codec static members
    let inline getCodec<'Encoding, .. when 'Encoding :> IEncoding and 'Encoding : struct> () : Codec<'Encoding, 't> =
        GetCodec.Invoke<'Encoding, OpCodec, 't> Unchecked.defaultof<'t>



    /// <summary>Derives a concrete field object codec for a required field.</summary>
    /// <param name="name">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline req (name: string) (getter: 'T -> 'param option) : Codec<MultiObj<'Encoding>, MultiObj<'Encoding>, 'param, 'T> = reqWithLazy (getCodec<'Encoding, 'param>) name getter



    /// <summary>Same as opt but using an explicit codec.</summary>
    let optWith c (prop: string) (getter: 'T -> 'Value option) =
        let getFromListOptWith decoder (m: MultiObj<_>) key =
            match m.[key] with
            | []        -> Ok None
            | value:: _ -> decoder value |> Result.map Some
        {
            Decoder = fun (o: MultiObj<'S>) -> getFromListOptWith (dec c) o prop
            Encoder = fun x -> (match getter x with Some (x: 'Value) -> multiMap [KeyValuePair (prop, (enc c) x)] | _ -> zero)
        }

    /// Derives a concrete field codec for an optional field
    let inline opt prop (getter: 'T -> 'param option) : Codec<MultiObj<'Encoding>, MultiObj<'Encoding>, 'param option, 'T> = optWith (getCodec<'Encoding, 'param> ()) prop getter






    // Applicative Codec operator and Computation Expression for specialized Codecs

    let privReturn f = ({ Decoder = (fun _ -> Ok f); Encoder = zero }) : Codec<MultiObj<'S>,MultiObj<'S>,_,_>
    let privlift2 (f: 'x ->'y ->'r) (x: Codec<MultiObj<'S>, MultiObj<'S>,'x,'T>) (y:  Codec<MultiObj<'S>, MultiObj<'S>,'y,'T>) : Codec<MultiObj<'S>, MultiObj<'S>,'r,'T> =
            {
                Decoder = fun s -> lift2 f (x.Decoder s) (y.Decoder s)
                Encoder = x.Encoder ++ y.Encoder
            }
    let privlift3 (f: 'x -> 'y -> 'z -> 'r) (x: Codec<MultiObj<'S>, MultiObj<'S>,'x,'T>) (y: Codec<MultiObj<'S>, MultiObj<'S>,'y,'T>) (z: Codec<MultiObj<'S>, MultiObj<'S>,'z,'T>) : Codec<MultiObj<'S>, MultiObj<'S>,'r,'T> =
            {
                Decoder = fun s -> lift3 f (x.Decoder s) (y.Decoder s) (z.Decoder s)
                Encoder = x.Encoder ++ y.Encoder ++ z.Encoder
            }


    type CodecBuilder<'t> () =

        member _.Delay x = x ()
        member _.ReturnFrom expr = expr
        member _.Return (x) = privReturn x
        member _.Yield  (x) : Codec<MultiObj<'r>, 't> = x
        member _.MergeSources  (t1, t2)     = privlift2 tuple2 t1 t2
        member _.MergeSources3 (t1, t2, t3) = privlift3 tuple3 t1 t2 t3
        member _.BindReturn (x: Codec<MultiObj<'r>, MultiObj<'r>,_,_>, f) = f <!> x
        member _.Run x : Codec<MultiObj<_>,'t> = x
        member _.Combine (x: Codec<MultiObj<'S>, 't>, y: Codec<MultiObj<'S>, 't>) = x <|> y : Codec<MultiObj<'S>, 't>


        // We can remove this with F# 5
        [<CompilerMessage("A Codec doesn't support the Zero operation.", 10708, IsError = true)>]
        member _.Zero () = raise <| new System.InvalidOperationException "This execution path is unreachable."

    /// Codec Applicative Computation Expression.
    let codec<'t> = CodecBuilder<'t> ()



    /// This is the entry point to register codecs for interface implementations.
    let initializeInterfaceImplementation<'Encoding, 'Interface, 'Type when 'Encoding :> IEncoding and 'Encoding : struct> (codec: unit -> Codec<MultiObj<'Encoding>, 'Type>) =
        let codec () =
            let objCodec = codec ()
            let (d, e) = objCodec.Decoder, objCodec.Encoder
            let nd = d >> Result.map (fun (x: 'Type) -> retype x : 'Interface)
            let ne =
                fun (x: 'Interface) ->
                    match box x with
                        | :? 'Type as t -> e t
                        | _ -> zero
            { Decoder = nd; Encoder = ne }
        codec |> CodecCollection<'Encoding, 'Interface>.AddSubtype typeof<'Type>

(* 

What should we do with lenses? Should we add a generic implementation?

*)