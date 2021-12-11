namespace Fleece

#nowarn "00042" // retype

open System
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data

/// Marker interface for all interfaces whose derived classes will support codecs
type ICodecInterface<'Base> = interface end

module Config =
    let mutable codecCacheEnabled = false

type PropertyList<'Encoding> (properties: (string * 'Encoding) []) =
    let properties = properties
    member _.Properties = properties
    member _.Item with get (key: string) = properties |> Seq.filter (fun (k, _) -> k = key) |> Seq.map snd |> Seq.toList
    member _.Count = properties.Length
    with
        interface System.Collections.IEnumerable with
            member _.GetEnumerator () = (properties |> Seq.map KeyValuePair).GetEnumerator () :> System.Collections.IEnumerator

        interface IEnumerable<KeyValuePair<string, 'Encoding>> with
            member _.GetEnumerator () = (properties |> Seq.map KeyValuePair).GetEnumerator ()

        interface IReadOnlyCollection<KeyValuePair<string,'Encoding>> with
            member _.Count = properties.Length
    
        interface IReadOnlyDictionary<string, 'Encoding> with
            member _.Keys = properties |> Seq.map fst
            member _.Values = properties |> Seq.map snd
            member _.Item with get (key: string) = properties |> Array.find (fun (k, _) -> k = key) |> snd
            member _.ContainsKey (key: string) = properties |> Array.exists (fun (k, _) -> k = key)
            member _.TryGetValue (key: string, value: byref<'Encoding>) =
                match properties |> Array.tryFindIndex (fun (k, _) -> k = key) with
                | Some i ->
                    value <- snd properties.[i]
                    true
                | None -> false

        static member Filter (x: PropertyList<'Encoding>, f) = x.Properties |> Array.filter f |> PropertyList
        static member get_Zero () = PropertyList [||]
        static member (+) (x: PropertyList<'Encoding>, y: PropertyList<'Encoding>) = PropertyList (x.Properties ++ y.Properties)
        static member Map (x: PropertyList<'Encoding>, f) = PropertyList (x.Properties |> map (fun (k, v) -> (k, f v)))
        static member ToSeq  (x: PropertyList<'Encoding>) = toSeq x.Properties
        static member ToList (x: PropertyList<'Encoding>) = toList x.Properties
        static member ToArray (x: PropertyList<'Encoding>) = x.Properties
        static member add key x (t: PropertyList<'Encoding>) =
            let i = t.Properties |> Array.tryFindIndex (fun (k, _) -> k = key)
            match i with
            | Some i ->
                let t = t.Properties |> Array.copy
                t.[i] <- (key, x)
                PropertyList t
            | None   -> PropertyList (t.Properties ++ [|(key, x)|])

[<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>]
module Helpers =

    let inline retype<'sourceType, 'destType> (x: 'sourceType) : 'destType =
    #if !FABLE_COMPILER
        (# "" x : 'destType #)
    #else
        unbox<'destType> x
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

    module Dictionary =

        let ofSeq (source: seq<'Key * 'T>) =
            let dct = Dictionary ()
            for (k, v) in source do
                dct.Add (k, v)
            dct

        let toSeq (source: Dictionary<'Key, 'T>) = seq {
            for (KeyValue (k, v)) in source do
                yield (k, v) }

        let toArray (source: Dictionary<'Key, 'T>) = toSeq source |> Seq.toArray

    let decoderNotAvailable (_: 'Encoding) : Result<'T, _> = failwithf "Fleece internal error: this codec has no decoder from encoding %A to type %A." typeof<'Encoding> typeof<'T>
    let encoderNotAvailable (_: 'T) : 'Encoding            = failwithf "Fleece internal error: this codec has no encoder from type %A to encoding %A." typeof<'T> typeof<'Encoding>

open Helpers


/// Encodes a value of a generic type 't into a value of raw type 'S.
type Encoder<'S, 't> = 't -> 'S

/// A decoder from raw type 'S1 and encoder to raw type 'S2 for strong types 't1 and 't2.
type Codec<'S1, 'S2, 't1, 't2> = { Decoder : Decoder<'S1, 't1>; Encoder : Encoder<'S2, 't2> } with
    static member inline Return f = { Decoder = (fun _ -> Ok f); Encoder = zero }


/// A codec for raw type 'S to strong type 't.
and Codec<'S, 't> = Codec<'S, 'S, 't, 't>

/// Decodes a value of raw type 'S into a value of generic type 't, possibly returning an error.
and Decoder<'S, 't> = 'S -> Result<'t, DecodeError>

and ParseResult<'t> = Result<'t, DecodeError>

and IEncoding =
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
    abstract propertyList   : Codec<IEncoding, 't>  -> Codec<IEncoding, PropertyList<'t>>
    abstract tuple1         : Codec<IEncoding, 't>  -> Codec<IEncoding, Tuple<'t>>
    abstract tuple2         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't1 * 't2>
    abstract tuple3         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't1 * 't2 * 't3>
    abstract tuple4         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4>
    abstract tuple5         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5>
    abstract tuple6         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't6> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5 * 't6>
    abstract tuple7         : Codec<IEncoding, 't1> -> Codec<IEncoding, 't2> -> Codec<IEncoding, 't3> -> Codec<IEncoding, 't4> -> Codec<IEncoding, 't5> -> Codec<IEncoding, 't6> -> Codec<IEncoding, 't7> -> Codec<IEncoding, 't1 * 't2 * 't3 * 't4 * 't5 * 't6 * 't7>    
    abstract enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> : unit -> Codec<IEncoding, 't>

    /// Returns a string representing the internal "case" (or type) of the encoding (ie: Array, Object, ... )
    abstract getCase : string

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

[<Struct>]
type AdHocEncoding = AdHocEncoding of AdHocEncodingPassing: (IEncoding -> IEncoding) with

    static member ofIEncoding (c1: Codec<IEncoding, 'T>) : _ -> Codec<IEncoding, 'T> =
        let dec1 (x: IEncoding)   = c1.Decoder (AdHocEncoding (fun _ -> x) :> IEncoding)
        let enc1 (i: IEncoding) v = let (AdHocEncoding x) = c1.Encoder v :?> AdHocEncoding in x i
        let codec1 i = { Decoder = dec1; Encoder = enc1 i }
        codec1

    static member ($) (_: AdHocEncoding, (x1, x2)                    ) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x)
    static member ($) (_: AdHocEncoding, (x1, x2, x3)                ) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x, AdHocEncoding.ofIEncoding x3 x)
    static member ($) (_: AdHocEncoding, (x1, x2, x3, x4)            ) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x, AdHocEncoding.ofIEncoding x3 x, AdHocEncoding.ofIEncoding x4 x)
    static member ($) (_: AdHocEncoding, (x1, x2, x3, x4, x5)        ) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x, AdHocEncoding.ofIEncoding x3 x, AdHocEncoding.ofIEncoding x4 x, AdHocEncoding.ofIEncoding x5 x)
    static member ($) (_: AdHocEncoding, (x1, x2, x3, x4, x5, x6)    ) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x, AdHocEncoding.ofIEncoding x3 x, AdHocEncoding.ofIEncoding x4 x, AdHocEncoding.ofIEncoding x5 x, AdHocEncoding.ofIEncoding x6 x)
    static member ($) (_: AdHocEncoding, (x1, x2, x3, x4, x5, x6, x7)) = fun x -> (AdHocEncoding.ofIEncoding x1 x, AdHocEncoding.ofIEncoding x2 x, AdHocEncoding.ofIEncoding x3 x, AdHocEncoding.ofIEncoding x4 x, AdHocEncoding.ofIEncoding x5 x, AdHocEncoding.ofIEncoding x6 x, AdHocEncoding.ofIEncoding x7 x)

    /// Evals the IEncoding parameter to get a concrete Codec.
    static member toIEncoding (codec: IEncoding -> Codec<IEncoding, 't>) : Codec<IEncoding, 't> =
        {
            Decoder = fun (x: IEncoding) ->
                let (AdHocEncoding x) = x :?> AdHocEncoding
                let i = x Unchecked.defaultof<_>
                (codec i).Decoder i
            Encoder = fun x -> AdHocEncoding (fun i -> (codec i).Encoder x) :> IEncoding
        }

    /// Same as toIEncoding but with one parameter.
    static member toIEncoding1 (codec: IEncoding -> _) codec1 =
        let codec1 x = AdHocEncoding.ofIEncoding codec1 x
        let codec s = (codec s) (codec1 s)
        AdHocEncoding.toIEncoding codec

    /// Same as toIEncoding but with many parameters in tupled form.
    static member inline toIEncodingN (codec: IEncoding -> _) tupledCodecs =
        let codecs = Unchecked.defaultof<AdHocEncoding> $ tupledCodecs
        let codec s = uncurryN (codec s) (codecs s)
        AdHocEncoding.toIEncoding codec

    interface IEncoding with
        member _.unit           = AdHocEncoding.toIEncoding (fun x -> x.unit)
        member _.boolean        = AdHocEncoding.toIEncoding (fun x -> x.boolean)
        member _.string         = AdHocEncoding.toIEncoding (fun x -> x.string)
        member _.dateTime       = AdHocEncoding.toIEncoding (fun x -> x.dateTime)
        member _.dateTimeOffset = AdHocEncoding.toIEncoding (fun x -> x.dateTimeOffset)
        member _.timeSpan       = AdHocEncoding.toIEncoding (fun x -> x.timeSpan)
        member _.decimal        = AdHocEncoding.toIEncoding (fun x -> x.decimal)
        member _.float          = AdHocEncoding.toIEncoding (fun x -> x.float)
        member _.float32        = AdHocEncoding.toIEncoding (fun x -> x.float32)
        member _.int            = AdHocEncoding.toIEncoding (fun x -> x.int)
        member _.uint32         = AdHocEncoding.toIEncoding (fun x -> x.uint32)
        member _.int64          = AdHocEncoding.toIEncoding (fun x -> x.int64)
        member _.uint64         = AdHocEncoding.toIEncoding (fun x -> x.uint64)
        member _.int16          = AdHocEncoding.toIEncoding (fun x -> x.int16)
        member _.uint16         = AdHocEncoding.toIEncoding (fun x -> x.uint16)
        member _.byte           = AdHocEncoding.toIEncoding (fun x -> x.byte)
        member _.sbyte          = AdHocEncoding.toIEncoding (fun x -> x.sbyte)
        member _.char           = AdHocEncoding.toIEncoding (fun x -> x.char)
        member _.guid           = AdHocEncoding.toIEncoding (fun x -> x.guid)
        member _.enum<'t, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType> () : Codec<IEncoding, 't> = AdHocEncoding.toIEncoding (fun x -> x.enum ())

        member _.result c1 c2     = AdHocEncoding.toIEncodingN (fun x -> x.result)  (c1, c2)
        member _.choice c1 c2     = AdHocEncoding.toIEncodingN (fun x -> x.choice)  (c1, c2)
        member _.choice3 c1 c2 c3 = AdHocEncoding.toIEncodingN (fun x -> x.choice3) (c1, c2, c3)
        member _.option c         = AdHocEncoding.toIEncoding1 (fun x -> x.option) c
        member _.array c          = AdHocEncoding.toIEncoding1 (fun x -> x.array)  c
        member _.propertyList c   = AdHocEncoding.toIEncoding1 (fun x -> x.propertyList) c

        member _.tuple1 c                    = AdHocEncoding.toIEncoding1 (fun x -> x.tuple1) c
        member _.tuple2 c1 c2                = AdHocEncoding.toIEncodingN (fun x -> x.tuple2) (c1, c2)
        member _.tuple3 c1 c2 c3             = AdHocEncoding.toIEncodingN (fun x -> x.tuple3) (c1, c2, c3)
        member _.tuple4 c1 c2 c3 c4          = AdHocEncoding.toIEncodingN (fun x -> x.tuple4) (c1, c2, c3, c4)
        member _.tuple5 c1 c2 c3 c4 c5       = AdHocEncoding.toIEncodingN (fun x -> x.tuple5) (c1, c2, c3, c4, c5)
        member _.tuple6 c1 c2 c3 c4 c5 c6    = AdHocEncoding.toIEncodingN (fun x -> x.tuple6) (c1, c2, c3, c4, c5, c6)
        member _.tuple7 c1 c2 c3 c4 c5 c6 c7 = AdHocEncoding.toIEncodingN (fun x -> x.tuple7) (c1, c2, c3, c4, c5, c6, c7)

        member x.getCase =
            // Normally it won't get called as errors will access the getCase from the wrapped AdHocEncoding
            let (AdHocEncoding f) = x
            let i = f Unchecked.defaultof<IEncoding>
            if not (Object.ReferenceEquals (i, null)) then i.getCase
            else "Unknown case"

/// Functions operating on Codecs
module Codec =

    let decode { Decoder = d } = d
    let encode { Encoder = e } = e

    /// Turns a Codec into another Codec, by mapping it over an isomorphism.
    let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) c =
        let { Decoder = r; Encoder = w } = c
        { Decoder = contramap f r; Encoder = map g w }

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
        { Decoder = dec1 >> (=<<) dec2 ; Encoder = enc1 << enc2 }

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



/// Helpers to deal with Decode errors.
module Decode =
    let inline Success x = Ok x : ParseResult<_>
    let (|Success|Failure|) (x: ParseResult<_>) = x |> function
        | Ok    x -> Success x
        | Error x -> Failure x

    module Fail =
        let inline objExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (DecodeError.EncodingCaseMismatch (typeof<'t>, v, "Object", a))
        let inline arrExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (DecodeError.EncodingCaseMismatch (typeof<'t>, v, "Array" , a))
        let inline numExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (DecodeError.EncodingCaseMismatch (typeof<'t>, v, "Number", a))
        let inline strExpected  (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (DecodeError.EncodingCaseMismatch (typeof<'t>, v, "String", a))
        let inline boolExpected (v: 'Encoding) : Result<'t, _> = let a = (v :> IEncoding).getCase in Error (DecodeError.EncodingCaseMismatch (typeof<'t>, v, "Bool"  , a))
        let [<GeneralizableValue>]nullString<'t> : Result<'t, _> = Error (DecodeError.NullString typeof<'t>)
        let inline count e (a: 'Encoding) = Error (DecodeError.IndexOutOfRange (e, a))
        let invalidValue (v: 'Encoding) o : Result<'t, _> = Error (DecodeError.InvalidValue (typeof<'t>, v, o))
        let propertyNotFound p (o: PropertyList<'Encoding>) = Error (DecodeError.PropertyNotFound (p, map (fun x -> x :> IEncoding) o))
        let parseError s v : Result<'t, _> = Error (DecodeError.ParseError (typeof<'t>, s, v))



module Codecs =

    let private instance<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> = new 'Encoding ()
    let private (<->) decoder encoder : Codec<_, _> = { Decoder = decoder; Encoder = encoder }

    let [<GeneralizableValue>] unit<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>           = instance<'Encoding>.unit           |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] boolean<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>        = instance<'Encoding>.boolean        |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] guid<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>           = instance<'Encoding>.guid           |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] char<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>           = instance<'Encoding>.char           |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] byte<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>           = instance<'Encoding>.byte           |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] sbyte<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>          = instance<'Encoding>.sbyte          |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] uint16<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>         = instance<'Encoding>.uint16         |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] uint32<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>         = instance<'Encoding>.uint32         |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] uint64<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>         = instance<'Encoding>.uint64         |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] int16<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>          = instance<'Encoding>.int16          |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] int<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>            = instance<'Encoding>.int            |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] int64<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>          = instance<'Encoding>.int64          |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] decimal<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>        = instance<'Encoding>.decimal        |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] float32<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>        = instance<'Encoding>.float32        |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] float<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>          = instance<'Encoding>.float          |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] string<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>         = instance<'Encoding>.string         |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] dateTime<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>       = instance<'Encoding>.dateTime       |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] dateTimeOffset<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> = instance<'Encoding>.dateTimeOffset |> Codec.downCast : Codec<'Encoding, _>
    let [<GeneralizableValue>] timeSpan<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)>       = instance<'Encoding>.timeSpan       |> Codec.downCast : Codec<'Encoding, _>
    let array        (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.array (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, array<'a>>
    let list         (codec: Codec<'Encoding, 'a>) = (Ok << Array.toList <-> Array.ofList) >.> array codec
    let set          (codec: Codec<'Encoding, 'a>) = (Ok << Set <-> Array.ofSeq)           >.> array codec
    let nonEmptyList (codec: Codec<'Encoding, 'a>) = (Array.toList >> NonEmptyList.tryOfList >> Option.toResultWith (DecodeError.Uncategorized "List is empty") <-> Array.ofSeq) >.> array codec
    let nonEmptySet  (codec: Codec<'Encoding, 'a>) = (Set >> NonEmptySet.tryOfSet >> Option.toResultWith (DecodeError.Uncategorized "Set is empty") <-> Array.ofSeq) >.> array codec
    let resizeArray  (codec: Codec<'Encoding, 'a>) = Codec.compose (array codec) (Ok << ResizeArray <-> Array.ofSeq)
    let propList     (codec: Codec<'Encoding, 'a>) = instance<'Encoding>.propertyList (Codec.upCast codec) |> Codec.downCast : Codec<'Encoding, PropertyList<'a>>
    let propMap      (codec: Codec<'Encoding, 'a>) = (Ok << Map.ofSeq << PropertyList.ToSeq <-> (Map.toArray >> PropertyList)) >.> propList codec
    let propDictionary  (codec: Codec<'Encoding, 'a>) = (Ok << Dictionary.ofSeq << PropertyList.ToSeq <-> (Dictionary.toArray >> PropertyList)) >.> propList codec
    let nonEmptyPropMap (codec: Codec<'Encoding, 'a>) = (PropertyList.ToSeq >> Map.ofSeq >> NonEmptyMap.tryOfMap >> Option.toResultWith (DecodeError.Uncategorized "Map is empty") <-> (NonEmptyMap.toArray >> PropertyList)) >.> propList codec
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
    let enum<'Encoding, 't, 'u when 't : enum<'u> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType and 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> = instance<'Encoding>.enum () |> Codec.downCast : Codec<'Encoding, 't>
    let [<GeneralizableValue>] base64Bytes<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> = (Ok << Convert.FromBase64String <-> Convert.ToBase64String) >.> instance<'Encoding>.string |> Codec.downCast : Codec<'Encoding, _>
    let id: Codec<'T, 'T> = { Decoder = Ok; Encoder = id }

    #if !FABLE_COMPILER
    let arraySegment (codec: Codec<'Encoding, 'a>) = (Ok << ArraySegment<_> << Seq.toArray <-> ArraySegment.toArray) >.> array codec
    #endif

    let dictionary (keyCodec: Codec<'Encoding, 'Key>) (valueCodec: Codec<'Encoding, 'Value>) : Codec<'Encoding, Dictionary<'Key, 'Value>> =
        let c = list (tuple2 keyCodec valueCodec)
        ((Ok << Dictionary.ofSeq) <-> (Dictionary.toSeq >> toList)) >.> c

    let map (keyCodec: Codec<'Encoding, 'Key>) (valueCodec: Codec<'Encoding, 'Value>) : Codec<'Encoding, Map<'Key, 'Value>> =
        let c = list (tuple2 keyCodec valueCodec)
        ((Ok << Map.ofList) <-> Map.toList) >.> c

    let nonEmptyMap (keyCodec: Codec<'Encoding, 'Key>) (valueCodec: Codec<'Encoding, 'Value>) : Codec<'Encoding, NonEmptyMap<'Key, 'Value>> =
        let c = list (tuple2 keyCodec valueCodec)
        ((Map.ofList >> NonEmptyMap.tryOfMap >> Option.toResultWith (DecodeError.Uncategorized "Map is empty")) <-> NonEmptyMap.toList) >.> c


[<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>]
module Internals =

    type Id1<'t> (v: 't) =
        let value = v
        member _.getValue = value

    type Id2<'t> (v: 't) =
        let value = v
        member _.getValue = value

    type IDefault7 = interface end
    type IDefault6 = interface inherit IDefault7 end
    type IDefault5 = interface inherit IDefault6 end
    type IDefault4 = interface inherit IDefault5 end
    type IDefault3 = interface inherit IDefault4 end
    type IDefault2 = interface inherit IDefault3 end
    type IDefault1 = interface inherit IDefault2 end
    type IDefault0 = interface inherit IDefault1 end

    type OpCodec  = OpCodec
    type OpEncode = OpEncode
    type OpDecode = OpDecode
    
    let mutable private codecCollectionGlobalVersion : uint = 0u
    let private codecCollectionMonitor = obj ()

    type CodecCollection<'Encoding, 'Interface> () =
        static let mutable subtypes : Dictionary<Type, unit -> Codec<PropertyList<'Encoding>, 'Interface>> = new Dictionary<_,_> ()
        static member GetSubtypes = subtypes
        static member AddSubtype ty (x: unit -> Codec<PropertyList<'Encoding>, 'Interface>) =
            lock codecCollectionMonitor
                (fun () ->
                    subtypes.[ty] <- x
                    codecCollectionGlobalVersion <- codecCollectionGlobalVersion + 1u)

    type CodecCache<'Operation, 'Encoding, 'T> () =
        static let mutable cachedCodecInterface : option<uint * Codec<'Encoding, 'T>> = None
        static let mutable cachedCodec          : option<       Codec<'Encoding, 'T>> = None

        static member GetCache () = cachedCodec
        static member Run (f: unit -> Codec<'Encoding, 'T>) =
            if not Config.codecCacheEnabled then f ()
            else
                match cachedCodec with
                | Some c -> c
                | None   ->
                    let c = f ()
                    cachedCodec <- Some c
                    c

        static member RunForInterfaces (f: unit -> Codec<'Encoding, 'T>) =
            if not Config.codecCacheEnabled then f ()
            else
                match cachedCodecInterface with
                | Some (version, c) when version = codecCollectionGlobalVersion -> c
                | _ ->
                    let version = codecCollectionGlobalVersion
                    let c = f ()
                    cachedCodecInterface <- Some (version, c)
                    c

    type GetCodec =
        interface IDefault0

        static member GetCodec (_: bool          , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.boolean       ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: string        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.string        ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: DateTime      , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.dateTime      ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: DateTimeOffset, _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.dateTimeOffset) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: TimeSpan      , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.timeSpan      ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: decimal       , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.decimal       ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: Double        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.float         ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: Single        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.float32       ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: int           , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.int           ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: uint32        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.uint32        ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: int64         , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.int64         ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: uint64        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.uint64        ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: int16         , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.int16         ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: uint16        , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.uint16        ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: byte          , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.byte          ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: sbyte         , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.sbyte         ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: char          , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.char          ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (_: Guid          , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.guid          ) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member GetCodec (()               , _: GetCodec, _, _: 'Operation) : Codec<'Encoding, _> = (fun () -> Codecs.unit          ) |> CodecCache<OpCodec, 'Encoding, _>.Run

        // Dummy overloads
        static member GetCodec (_: OpCodec , _: GetCodec, _, _: OpEncode) = invalidOp "Fleece internal error: this code should be unreachable." : Codec<'Encoding, OpCodec>
        static member GetCodec (_: OpEncode, _: GetCodec, _, _: OpEncode) = invalidOp "Fleece internal error: this code should be unreachable." : Codec<'Encoding, OpEncode>

        /// Invoker for Codec
        static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> (x: 't) : Codec<'Encoding, ^t> =
            let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
            call (Unchecked.defaultof<GetCodec>, x)

    type GetDec =
        inherit GetCodec
     
        /// Invoker for Codec, originated from a Decoder Invoker.
        static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> (x: 't) : Codec<'Encoding, ^t> =
            let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
            call (Unchecked.defaultof<GetDec>, x)

    type GetEnc =
        inherit GetDec

        /// Recursive Invoker
        static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> (x: 't, _: unit) : Codec<'Encoding, ^t> =
            let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
            call (Unchecked.defaultof<GetEnc>, x)

        /// Invoker for Codec, originated from an Encoder Invoker.
        static member inline Invoke<'Encoding, 'Operation, .. when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> (x: 't) : Codec<'Encoding, ^t> =
            let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member GetCodec: ^b * ^a* ^a * _ -> Codec<'Encoding, ^t>) b, a, a, Unchecked.defaultof<'Operation>)
            call (Unchecked.defaultof<GetEnc>, x)

    type GetCodec with
        static member inline GetCodec (_: Tuple<'a> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Tuple<'a>> = Codecs.tuple1 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
        static member inline GetCodec (_: 'a Id1    when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, _, _: 'Operation) = Ok (Id1<'a> Unchecked.defaultof<'a>), Map.empty

    type GetCodec with
        static member inline GetCodec (_:'tuple when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, _> =
            fun () -> 
                let { Decoder = ofArray; Encoder = toArray } = Codecs.array Codecs.id
                let c1 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t1>
                let c2 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t2>
                let c3 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t3>
                let c4 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t4>
                let c5 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t5>
                let c6 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t6>
                let c7 = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'t7>
                let cr = GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'tr>
                { 
                    Decoder = fun x ->
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
                            | Error (DecodeError.IndexOutOfRange (i, _)) -> Error (DecodeError.IndexOutOfRange (i + 8, x))
                            | _ -> curryN (Tuple<_,_,_,_,_,_,_,_> >> retype : _ -> 'tuple) <!> t1 <*> t2 <*> t3 <*> t4 <*> t5 <*> t6 <*> t7 <*> tr
                        | Error e -> Error e
                    Encoder = fun (t: 'tuple) ->
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
                }
            |> CodecCache<OpCodec, 'Encoding, 'tuple>.Run

    type GetCodec with static member inline GetCodec (_: Result<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Result<'a,'b>> = (fun () -> Codecs.result (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: Choice<'a, 'b> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Choice<'a,'b>> = (fun () -> Codecs.choice (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: Choice<'a, 'b, 'c> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Choice<'a,'b,'c>> = (fun () -> Codecs.choice3 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a option   when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, option<'a>>   = (fun () -> Codecs.option   (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a Nullable when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Nullable<'a>> = (fun () -> Codecs.nullable (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: NonEmptyList<'T> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, NonEmptyList<'T>> = (fun () -> Codecs.nonEmptyList (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'T>)) |> CodecCache<OpCodec, 'Encoding, _>.Run

    type GetCodec with
        static member inline GetCodec (_: 'a array  when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, array<'a>> = (fun () -> Codecs.array  (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run

        #if !FABLE_COMPILER
        static member inline GetCodec (_: ArraySegment<'a>  when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, ArraySegment<'a>> = (fun () -> Codecs.arraySegment (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
        #endif

    type GetCodec with static member inline GetCodec (_: list<'a>        when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _          , c, _: 'Operation) : Codec<'Encoding, list<'a>>        = (fun () -> Codecs.list        (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: Set<'a>         when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Set<'a>>         = (fun () -> Codecs.set         (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: NonEmptySet<'a> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, NonEmptySet<'a>> = (fun () -> Codecs.nonEmptySet (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with
        static member inline GetCodec (_: Map<'K, 'V> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Map<'K, 'V>> =
            fun () ->
                match typeof<'K> with
                | t when t = typeof<string> 
                    -> Codecs.propMap (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>) |> retype
                | _ -> Codecs.map     (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'K>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>)
            |> CodecCache<OpCodec, 'Encoding, _>.Run
        
    type GetCodec with
        static member inline GetCodec (_: PropertyList<'a> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, PropertyList<'a>> =
            fun () -> Codecs.propList (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)
            |> CodecCache<OpCodec, 'Encoding, _>.Run

        static member inline GetCodec (_: NonEmptyMap<'K, 'V> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, NonEmptyMap<'K, 'V>> =
            fun () -> 
                match typeof<'K> with
                | t when t = typeof<string> 
                    -> Codecs.nonEmptyPropMap (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>) |> retype
                | _ -> Codecs.nonEmptyMap     (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'K>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>)
            |> CodecCache<OpCodec, 'Encoding, _>.Run

        static member inline GetCodec (_: Dictionary<'K, 'V> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, Dictionary<'K, 'V>> =
            fun () ->
                match typeof<'K> with
                | t when t = typeof<string> 
                    -> Codecs.propDictionary (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>) |> retype
                | _ -> Codecs.dictionary     (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'K>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'V>)
            |> CodecCache<OpCodec, 'Encoding, _>.Run
        
        static member inline GetCodec (_: ResizeArray<'a>         when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, ResizeArray<'a>>        = (fun () -> Codecs.resizeArray     (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>)) |> CodecCache<OpCodec, 'Encoding, _>.Run
        static member inline GetCodec (_: 'a Id2   when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, _, _: 'Operation)  = (Ok (Id2<'a> Unchecked.defaultof<'a>)), Map.empty

    type GetCodec with static member inline GetCodec (_: 'a * 'b                          when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b                         > = (fun () -> Codecs.tuple2 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>)                                                                                                                                                                                                                                                                                                                                          ) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c                     when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c                    > = (fun () -> Codecs.tuple3 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>)                                                                                                                                                                                                                                                                        ) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd                when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd               > = (fun () -> Codecs.tuple4 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>)                                                                                                                                                                                                      ) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e           when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e          > = (fun () -> Codecs.tuple5 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>)                                                                                                                                    ) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f      when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f     > = (fun () -> Codecs.tuple6 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'f>)                                                                  ) |> CodecCache<OpCodec, 'Encoding, _>.Run
    type GetCodec with static member inline GetCodec (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: GetCodec, c, _: 'Operation) : Codec<'Encoding, 'a * 'b * 'c * 'd * 'e * 'f * 'g> = (fun () -> Codecs.tuple7 (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'a>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'b>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'c>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'d>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'e>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'f>) (GetEnc.Invoke<'Encoding, 'Operation, _> Unchecked.defaultof<'g>)) |> CodecCache<OpCodec, 'Encoding, _>.Run

    type GetCodec with static member inline GetCodec (_: 't when 't : enum<_> and 't : (new : unit -> 't) and 't : struct and 't :> ValueType, _: GetCodec, c, _: 'Operation) = (fun () -> Codecs.enum) |> CodecCache<OpCodec, 'Encoding, _>.Run

    type GetCodec with

        // Overload to handle user-defined interfaces
        static member inline GetCodec (_: 'Base when 'Base :> ICodecInterface<'Base>, _: IDefault4, _, _: 'Operation) : Codec<'Encoding, 'Base> when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding) =
            fun () ->
                match CodecCollection<'Encoding, 'Base>.GetSubtypes |> toList |> NonEmptyList.tryOfList with
                | None ->
                    match CodecCollection<AdHocEncoding, 'Base>.GetSubtypes |> toList |> NonEmptyList.tryOfList with
                    | None -> failwithf "Unexpected error: codec list is empty for interface %A to Encoding %A." typeof<'Base> typeof<'Encoding>
                    | Some codecs ->
                        (codecs |> map (fun (KeyValue(_, x)) -> x ()) |> choice >.> Codecs.propList Codecs.id |> Codec.upCast |> AdHocEncoding.ofIEncoding) (new 'Encoding () :> IEncoding) |> Codec.downCast<_, 'Encoding>
                | Some cs -> cs |> map (fun (KeyValue(_, x)) -> x ()) |> choice >.> Codecs.propList Codecs.id
            |> CodecCache<OpCodec, 'Encoding, _>.RunForInterfaces

    type GetCodec with

        // Overload to "passthrough" an IEncoding
        static member GetCodec (_: 'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding), _: IDefault3, _, _: 'Operation) = Codecs.id : Codec<'Encoding, 'Encoding>
    
        // Main overload for external classes
        static member inline GetCodec (_: 'T, _: IDefault3, _, _: 'Operation) : Codec<'Encoding, 'T> =
            fun () ->
                (^T : (static member Codec: Codec< 'Encoding, 'T>) ())
            |> CodecCache<OpCodec, 'Encoding, 'T>.Run

        // Codec for specific 'Encoding
        static member inline GetCodec (_: 'T, _: IDefault4, _, _: 'Operation) =
            fun () ->
                let mutable r = Unchecked.defaultof<Codec< 'Encoding, 'T>>
                do (^T : (static member Codec : byref<Codec< 'Encoding, 'T>> -> unit) &r)
                r
            |> CodecCache<OpCodec, 'Encoding, 'T>.Run

        // For backwards compatibility
        // [<Obsolete("This function resolves to a deprecated 'JsonObjCodec' method and it won't be supported in future versions of this library. Please rename it to 'Codec' or 'get_Codec ()' and convert the result by applying the 'ofObjCodec' function.")>]
        // But adding the warning changes overload resolution.
        static member inline GetCodec (_: 'T, _: IDefault5, _, _: 'Operation) : Codec<'Encoding, 'T> =
            fun () ->
                let c: Codec<PropertyList<'Encoding>, 'T> = (^T : (static member JsonObjCodec: Codec<PropertyList<'Encoding>, 'T>) ())
                c >.> Codecs.propList Codecs.id
            |> CodecCache<OpCodec, 'Encoding, 'T>.Run

        // For specific 'Encoding in recursive calls coming from a get_Codec operation
        static member inline GetCodec (_: 'T, _: IDefault7, _, _: 'Operation) : Codec<'Encoding, 'T> =
            fun () ->
                let d j = (^T : (static member OfJson: 'Encoding -> ^T ParseResult) j) : ^T ParseResult
                let e t = (^T : (static member ToJson : ^T -> 'Encoding) t)
                { Decoder = d; Encoder = e }
            |> CodecCache<OpCodec, 'Encoding, 'T>.Run
    
        // For generic 'Encoding in recursive calls coming from a get_Codec operation
        static member inline GetCodec (_: 'T, _: IDefault6, _, _: 'Operation) : Codec<'Encoding, 'T> =
            fun () ->
                let d j = (^T : (static member OfJson: 'Encoding -> ^T ParseResult) j) : ^T ParseResult
                let e t =
                    let mutable r = Unchecked.defaultof<'Encoding>
                    let _ = (^T : (static member Encode : ^T * byref<'Encoding> -> unit) (t, &r))
                    r
                { Decoder = d; Encoder = e }
            |> CodecCache<OpCodec, 'Encoding, 'T>.Run


    type GetEnc with
        // Encoder for specific 'Encoding
        static member inline GetCodec (_: 'T, _: IDefault2, _: GetEnc, _: OpEncode) : Codec<'Encoding, ^T> =
            fun () ->
                let e t =
                    let mutable r = Unchecked.defaultof<'Encoding>
                    do (^T : (static member Encode : ^T * byref<'Encoding> -> unit) (t, &r))
                    r
                { Decoder = decoderNotAvailable; Encoder = e }
            |> CodecCache<OpEncode, 'Encoding, 'T>.Run

    type GetEnc with
        // Encoder for generic 'Encoding
        static member inline GetCodec (_: 'T, _: IDefault1, _: GetEnc, _: OpEncode) : Codec<'Encoding, ^T> =
            fun () ->
                let e t = (^T : (static member ToJson : ^T -> 'Encoding) t)
                { Decoder = decoderNotAvailable; Encoder = e }
            |> CodecCache<OpEncode, 'Encoding, 'T>.Run

    type GetDec with
        [<Obsolete("This function resolves to a deprecated 'OfJson' overload, returning a string as an error and it won't be supported in future versions of this library. Please update the 'OfJson' method, using the 'Fail' module to create a DecodeError.")>]
        static member inline GetCodec (_: 'T, _: IDefault1, _: GetDec, _: OpDecode) : Codec<'Encoding, ^T> =
            fun () ->
                let d j = Result.bindError (Error << DecodeError.Uncategorized) (^T : (static member OfJson: 'Encoding -> Result< ^T, string>) j)
                { Decoder = d; Encoder = encoderNotAvailable }
            |> CodecCache<OpDecode, 'Encoding, 'T>.Run

    type GetDec with
        // Decoder
        static member inline GetCodec (_: 'T, _: IDefault0, _: GetDec, _: OpDecode) : Codec<'Encoding, ^T> =
            fun () ->
                let d j = (^T : (static member OfJson: 'Encoding -> ^T ParseResult) j) : ^T ParseResult
                { Decoder = d; Encoder = encoderNotAvailable }
            |> CodecCache<OpDecode, 'Encoding, 'T>.Run
    

[<AutoOpen>]
module Operators =
    
    open Internals

    /// Creates a Codec from a pair of decoder and encoder functions, used mainly internally and in Encoding implementations.
    let (<->) decoder encoder : Codec<_,_,_,_> = { Decoder = decoder; Encoder = encoder }

    let (|Codec|) { Decoder = x; Encoder = y } = (x, y)

    let inline toEncoding< 'Encoding, .. when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> (x: 't) : 'Encoding =
        let codec = GetEnc.Invoke<'Encoding, OpEncode, _> x
        (codec |> Codec.encode) x

    let inline ofEncoding (x: 'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)) : Result<'t, _> =
        let codec = GetDec.Invoke<'Encoding, OpDecode, _> Unchecked.defaultof<'t>
        (codec |> Codec.decode) x
            
    /// Creates a codec to (from) 'Encoding from (to) an Object-Codec.
    /// <param name="objCodec">A codec of MultiMap from/to a strong type.</param>
    /// <returns>A codec of a strong type to (from) Encoding.</returns>
    let ofObjCodec (objCodec: Codec<PropertyList<'Encoding>, 't>) : Codec<_, 't> = objCodec >.> Codecs.propList Codecs.id


    let jreqWith (c: Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: PropertyList<_>) key =
            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key m
            | value:: _ -> decoder value
        {
            Decoder = fun (o: PropertyList<'Encoding>) -> getFromListWith (Codec.decode c) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> PropertyList [| prop, Codec.encode c x |] | _ -> zero
        }

    let jreqWithLazy (c: unit -> Codec<'Encoding,_,_,'Value>) (prop: string) (getter: 'T -> 'Value option) =
        let getFromListWith decoder (m: PropertyList<_>) key =
            match m.[key] with
            | []        -> Decode.Fail.propertyNotFound key m
            | value:: _ -> decoder value
        {
            Decoder = fun (o: PropertyList<'Encoding>) -> getFromListWith (Codec.decode (c ())) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> PropertyList [| prop, Codec.encode (c ()) x |] | _ -> zero
        }

    /// Derive automatically a Codec from the type, based on GetCodec / Codec static members.
    let inline defaultCodec<'Encoding, ^t when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding) and (GetCodec or ^t) : (static member GetCodec: ^t * GetCodec * GetCodec * OpCodec -> Codec<'Encoding, ^t>)> =
        GetCodec.Invoke<'Encoding, OpCodec, 't> Unchecked.defaultof<'t>



    /// <summary>Derives a concrete field object codec for a required field.</summary>
    /// <param name="name">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline jreq (name: string) (getter: 'T -> 'param option) : Codec<PropertyList<'Encoding>, PropertyList<'Encoding>, 'param, 'T> = jreqWithLazy (fun () -> defaultCodec<'Encoding, 'param>) name getter

    /// <summary>Same as jopt but using an explicit codec.</summary>
    let joptWith c (prop: string) (getter: 'T -> 'Value option) =
        let getFromListOptWith decoder (m: PropertyList<_>) key =
            match m.[key] with
            | []        -> Ok None
            | value:: _ -> decoder value |> Result.map Some
        {
            Decoder = fun (o: PropertyList<'S>) -> getFromListOptWith (Codec.decode c) o prop
            Encoder = fun x -> match getter x with Some (x: 'Value) -> PropertyList [| prop, Codec.encode c x |] | _ -> zero
        }

    /// Derives a concrete field codec for an optional field.
    let inline jopt prop (getter: 'T -> 'param option) : Codec<PropertyList<'Encoding>, PropertyList<'Encoding>, 'param option, 'T> = joptWith defaultCodec<'Encoding, 'param> prop getter


    let jobj (x: list<string * 'Encoding>) : 'Encoding = x |> List.toArray |> PropertyList |> Codec.encode (Codecs.propList Codecs.id)

    let JNull<'Encoding when 'Encoding :> IEncoding and 'Encoding : (new : unit -> 'Encoding)> : 'Encoding = (Codecs.option Codecs.unit |> Codec.encode) None
    let JBool   x = (Codecs.boolean |> Codec.encode) x
    let JNumber x = (Codecs.decimal |> Codec.encode) x
    let JString x = (Codecs.string  |> Codec.encode) x
    let JArray (x: IReadOnlyList<'Encoding>) = (Codecs.array Codecs.id |> Codec.encode) (toArray x)
    let JObject x = (Codecs.propList Codecs.id |> Codec.encode) x
    
    let (|JNull|_|)   (x: 'Encoding) = match (Codecs.option Codecs.id |> Codec.decode) x with | Ok None -> Some () | _ -> None    
    let (|JBool|_|)   (x: 'Encoding) = (Codecs.boolean |> Codec.decode) x |> Option.ofResult
    let (|JNumber|_|) (x: 'Encoding) = (Codecs.decimal |> Codec.decode) x |> Option.ofResult
    let (|JString|_|) (x: 'Encoding) = (Codecs.string  |> Codec.decode) x |> Option.ofResult
    let (|JArray|_|)  (x: 'Encoding) = (Codecs.array        Codecs.id |> Codec.decode) x |> Option.ofResult |> Option.map IReadOnlyList.ofArray
    let (|JObject|_|) (x: 'Encoding) = (Codecs.propList Codecs.id |> Codec.decode) x |> Option.ofResult

    
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
    open Internals
    type ICodecInterface<'Base> with
        /// This is the entry point to register codecs for interface implementations.
        static member RegisterCodec<'Encoding, 'Type> (codec: unit -> Codec<PropertyList<'Encoding>, 'Type>) =
            let codec () =
                let objCodec = codec ()
                let (d, e) = objCodec.Decoder, objCodec.Encoder
                let nd = d >> Result.map retype<'Type, 'Base>
                let ne (x: 'Base) =
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

        // Clients using F# lower than 5 will need this method
        [<CompilerMessage("A Codec doesn't support the Zero operation.", 10708, IsError = true)>]
        member _.Zero () = invalidOp "Fleece internal error: this code should be unreachable."

    /// Codec Applicative Computation Expression.
    let codec = CodecApplicativeBuilder ()


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
        let inline dkey i f t = map (fun x -> PropertyList.add i x t) (f (t.[i] |> function [] -> JNull | x::_ -> x))
        _JObject << dkey i

    let inline _jnth i =
        let inline dnth i f t = map (fun x -> t |> IReadOnlyList.trySetItem i x |> Option.defaultValue t) (f (IReadOnlyList.tryItem i t |> Option.defaultValue JNull))
        _JArray << dnth i

    // Reimport some basic Lens operations from F#+

    let setl optic value   (source: 's) : 't = setl optic value source
    let over optic updater (source: 's) : 't = over optic updater source
    let preview (optic: ('a -> Const<_,'b>) -> _ -> Const<_,'t>) (source: 's) : 'a option = preview optic source