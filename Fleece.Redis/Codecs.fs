namespace Fleece.Redis
open FSharpPlus
open FSharpPlus.Data
open StackExchange.Redis
open System

type Default7 = class end
type Default6 = class inherit Default7 end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

[<AutoOpen>]
module Redis=
    module Helpers =
        /// try to find entry with name equal to key
        let inline tryFindEntry (key:string) (o: HashEntry list) =
            let k :RedisValue= implicit key
            o |> List.tryFind (fun p -> p.Name.Equals(k) ) 
              |> Option.map (fun v-> v.Value)
        module HashEntryList=
            let union a b = List.append a b // NOTE: let's start with this, need to verify assumption later
    open Helpers
    type RObject = HashEntry list

    type DecodeError =
        | NullString of System.Type
        | IndexOutOfRange of int * RedisValue
        | InvalidValue of System.Type * RedisValue * string
        | PropertyNotFound of string * RObject
        | ParseError of System.Type * exn * string
        | Uncategorized of string
        | Multiple of DecodeError list

    with
      static member (+) (x, y) =
            match x, y with
            | Multiple x, Multiple y -> Multiple (x @ y)
            | _                      -> Multiple [x; y]
      override x.ToString () =
            match x with
            | NullString t -> sprintf "Expected %s, got null" (string t)
            | IndexOutOfRange (e, a) -> sprintf "Expected array with %s items, was: %s" (string e) (string a)
            | InvalidValue (t, v, s) -> sprintf "Value %s is invalid for %s%s" (string v) (string t) (if String.IsNullOrEmpty s then "" else " " + s)
            | PropertyNotFound (p, o) -> sprintf "Property: '%s' not found in object '%s'" p (string o)
            | ParseError (t, s, v) -> sprintf "Error decoding %s from  %s: %s" (string v) (string t) (string s)
            | Uncategorized str -> str
            | Multiple lst -> List.map string lst |> String.concat "\r\n"

    type 'a ParseResult = Result<'a, DecodeError>

    module Decode =
        let inline Success x = Ok x
        let (|Success|Failure|) = function
            | Ok    x -> Success x
            | Error x -> Failure x

        module Fail =
            let [<GeneralizableValue>]nullString<'t> : Result<'t, _> = Error (NullString typeof<'t>)
            let inline count e a = Error (IndexOutOfRange (e, a))
            let invalidValue v o : Result<'t, _> = Error (InvalidValue (typeof<'t>, v, o))
            let propertyNotFound p o = Error (PropertyNotFound (p, o))
            let parseError s v : Result<'t, _> = Error (ParseError (typeof<'t>, s, v))
    open Decode
    [<RequireQualifiedAccess>]
    module RedisEncode =

        let boolean        (x: bool          ) :RedisValue = implicit x
        let int            (x: int           ) :RedisValue = implicit x
        let int64          (x: int64         ) :RedisValue = implicit x
        let double         (x: double        ) :RedisValue = implicit x
        let string         (x: string        ) :RedisValue = implicit x
        let byteArray      (x: byte array    ) :RedisValue = implicit x
        let unit           (_: unit          ) :RedisValue = implicit [||]

    [<RequireQualifiedAccess>]
    module RedisDecode =
        module Helpers=
            let inline tryRead x =
                try
                  Success (explicit x)
                with
                | e -> Decode.Fail.invalidValue x (string e)
        open Helpers
        let boolean        (x: RedisValue     ) :bool ParseResult = tryRead x
        let int            (x: RedisValue     ) :int ParseResult  = tryRead x
        let int64          (x: RedisValue     ) :int64 ParseResult = tryRead x
        let double         (x: RedisValue     ) :double ParseResult = tryRead x
        let string         (x: RedisValue     ) :string ParseResult = tryRead x
        let byteArray      (x: RedisValue     ) :byte array ParseResult = tryRead x
        let unit           (x: RedisValue     ) :unit ParseResult = match tryRead x with | Success [||] -> Success () | _ -> Decode.Fail.invalidValue x ("Expected empty array")

    type OfRedis=
        inherit Default1
        static member OfRedis (_: bool,       _: OfRedis) = RedisDecode.boolean
        static member OfRedis (_: int,        _: OfRedis) = RedisDecode.int
        static member OfRedis (_: int64,      _: OfRedis) = RedisDecode.int64
        static member OfRedis (_: double,     _: OfRedis) = RedisDecode.double
        static member OfRedis (_: string,     _: OfRedis) = RedisDecode.string
        static member OfRedis (_: byte array, _: OfRedis) = RedisDecode.byteArray
        static member OfRedis (_: unit,       _: OfRedis) = RedisDecode.unit

    type OfRedis with
        static member inline Invoke (x: RedisValue) : 't ParseResult =
            let inline iOfRedis (a: ^a, b: ^b) = ((^a or ^b) : (static member OfRedis : ^b * _ -> (RedisValue -> ^b ParseResult)) b, a)
            iOfRedis (Unchecked.defaultof<OfRedis>, Unchecked.defaultof<'t>) x

    /// Maps Redis to a type
    let inline ofRedis (x: RedisValue) : 't ParseResult = OfRedis.Invoke x

    type ToRedis =
        inherit Default1
        static member ToRedis (x: bool          , _: ToRedis) = RedisEncode.boolean        x
        static member ToRedis (x: int           , _: ToRedis) = RedisEncode.int            x
        static member ToRedis (x: int64         , _: ToRedis) = RedisEncode.int64          x
        static member ToRedis (x: double        , _: ToRedis) = RedisEncode.double         x
        static member ToRedis (x: string        , _: ToRedis) = RedisEncode.string         x
        static member ToRedis (x: byte array    , _: ToRedis) = RedisEncode.byteArray      x
        static member ToRedis (x: unit          , _: ToRedis) = RedisEncode.unit           x

    type ToRedis with
        static member inline Invoke (x: 't) : RedisValue =
            let inline iToRedis (a: ^a, b: ^b) = ((^a or ^b) : (static member ToRedis : ^b * _ -> RedisValue) b, a)
            iToRedis (Unchecked.defaultof<ToRedis>, x)

    let inline toRedis (x: 't) : RedisValue = ToRedis.Invoke x
    

    /// Encodes a value of a generic type 't into a value of raw type 'S.
    type Encoder<'S, 't> = 't -> 'S

    /// Decodes a value of raw type 'S into a value of generic type 't, possibly returning an error.
    type Decoder<'S, 't> = 'S -> ParseResult<'t>

    /// A decoder from raw type 'S1 and encoder to raw type 'S2 for string types 't1 and 't2.
    type Codec<'S1, 'S2, 't1, 't2> = Decoder<'S1, 't1> * Encoder<'S2, 't2>

    /// A decoder from raw type 'S1 and encoder to raw type 'S2 for type 't.
    type Codec<'S1, 'S2, 't> = Codec<'S1, 'S2, 't, 't>

    /// A codec for raw type 'S decoding to strong type 't1 and encoding to strong type 't2.
    type SplitCodec<'S, 't1, 't2> = Codec<'S, 'S, 't1, 't2>

    /// A codec for raw type 'S to strong type 't.
    type Codec<'S, 't> = Codec<'S, 'S, 't>

    type ConcreteCodec<'S1, 'S2, 't1, 't2> = { Decoder : ReaderT<'S1, ParseResult<'t1>>; Encoder : 't2 -> Const<'S2, unit> } with
        static member inline Return f = { Decoder = result f; Encoder = konst <| result () }
        static member inline (<*>) (remainderFields: ConcreteCodec<'S, 'S, 'f ->'r, 'T>, currentField: ConcreteCodec<'S, 'S, 'f, 'T>) =
            {
                Decoder = (remainderFields.Decoder : ReaderT<'S, ParseResult<'f -> 'r>>) <*> currentField.Decoder
                Encoder = fun w -> (remainderFields.Encoder w *> currentField.Encoder w)
            }
        static member inline (<!>) (f, field: ConcreteCodec<'S, 'S, 'f, 'T>) = f <!> field
        static member inline (<|>) (source: ConcreteCodec<'S, 'S, 'f, 'T>, alternative: ConcreteCodec<'S, 'S, 'f, 'T>) =
            {
                Decoder = (source.Decoder : ReaderT<'S, ParseResult<'f>>) <|> alternative.Decoder
                Encoder = fun w -> (source.Encoder w ++ alternative.Encoder w)
            }

    /// Derive automatically a RedisCodec, based of OfRedis and ToRedis static members
    let inline redisValueCodec< ^t when (OfRedis or ^t) : (static member OfRedis : ^t * OfRedis -> (RedisValue -> ^t ParseResult)) and (ToRedis or ^t) : (static member ToRedis : ^t * ToRedis -> RedisValue)> : Codec<RedisValue,'t> = ofRedis, toRedis


    module Codec =
        /// Turns a Codec into another Codec, by mapping it over an isomorphism.
        let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) (r, w) = (contramap f r, map g w)

        let inline compose codec1 codec2 = 
            let (dec1, enc1) = codec1
            let (dec2, enc2) = codec2
            (dec1 >> (=<<) dec2, enc1 << enc2)

        let decode (d: Decoder<'i, 'a>, _) (i: 'i) : ParseResult<'a> = d i
        let encode (_, e: Encoder<'o, 'a>) (a: 'a) : 'o = e a

        let inline ofConcrete {Decoder = ReaderT d; Encoder = e} = contramap id d, map id (e >> Const.run)
        let inline toConcrete (d: _ -> _, e: _ -> _) = { Decoder = ReaderT (contramap id d); Encoder = Const << map id e }

    /// <summary>Initialize the field mappings.</summary>
    /// <param name="f">An object constructor as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let withFields f = (fun _ -> Success f), (fun _ -> [])

    let diApply combiner (remainderFields: SplitCodec<'S, 'f ->'r, 'T>) (currentField: SplitCodec<'S, 'f, 'T>) =
      ( 
          Compose.run (Compose (fst remainderFields: Decoder<'S, 'f -> 'r>) <*> Compose (fst currentField)),
          fun p -> combiner (snd remainderFields p) ((snd currentField) p)
      )

    /// Creates a new Redis key,value pair for a Redis object
    let inline rpairWith toRedis (key: string) value = HashEntry(implicit key, toRedis value) 

    /// Creates a new Redis key,value pair for a Redis object
    let inline rpair (key: string) value = rpairWith toRedis key value
    /// Creates a new Redis key,value pair for a Redis object if the value option is present
    let inline rpairOptWith toRedis (key: string) value = match value with Some value -> (key, toRedis value) | _ -> (null, RedisValue.Null)
    /// Creates a new Redis key,value pair for a Redis object if the value option is present
    let inline rpairOpt (key: string) value = rpairOptWith toRedis key value


    /// Gets a value from a Redis object
    let inline rgetWith ofRedis (o: RObject) key =
        match tryFindEntry key o with
        | Some value -> ofRedis value
        | _ -> Decode.Fail.propertyNotFound key o
    /// Gets a value from a Redis object
    let inline rget (o: RObject) key = rgetWith ofRedis o key

    // Tries to get a value from a Redis object.
    /// Returns None if key is not present in the object.
    let inline rgetOptWith ofRedis (o: RObject) key =
        match tryFindEntry key o with
        | Some value -> ofRedis value |> map Some
        | _ -> Success None
    /// Tries to get a value from a Redis object.
    /// Returns None if key is not present in the object.
    let inline rgetOpt (o: RObject) key = rgetOptWith ofRedis o key

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline rfieldWith codec fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _ -> 'Rest, _>) =
        let inline deriveFieldCodec codec prop getter =
            (
                (fun (o: RObject) -> rgetWith (fst codec) o prop),
                (getter >> fun (x: 'Value) -> [HashEntry(implicit prop, ((snd codec) x))])
            )
        diApply HashEntryList.union rest (deriveFieldCodec codec fieldName getter)

    /// <summary>Appends a field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline rfield fieldName (getter: 'T -> 'Value) (rest: SplitCodec<_, _ -> 'Rest, _>) = rfieldWith redisValueCodec fieldName getter rest

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="codec">The codec to be used.</param>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline rfieldOptWith codec fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _ -> 'Rest, _>) =
        let inline deriveFieldCodecOpt codec prop getter =
            (
                (fun (o: RObject) -> rgetOptWith (fst codec) o prop),
                (getter >> function Some (x: 'Value) -> [HashEntry(implicit prop, ((snd codec) x))] | _ -> [])
            )
        diApply HashEntryList.union rest (deriveFieldCodecOpt codec fieldName getter)

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline rfieldOpt fieldName (getter: 'T -> 'Value option) (rest: SplitCodec<_, _ -> 'Rest, _>) = rfieldOptWith redisValueCodec fieldName getter rest

module Operators =
    open Helpers
    /// Creates a new Redis HashEntry for a Redis hash entry list
    /// 
    let inline (.=) key value = rpair key value

    /// Creates a new Redis HashEntry for a Redis hash entry list if the value is present in the option
    let inline (.=?) (key: string) value = rpairOpt key value

    /// Gets a value from a Redis object
    let inline (.@) o key = rget o key

    /// Tries to get a value from a Redis object.
    /// Returns None if key is not present in the object.
    let inline (.@?) o key = rgetOpt o key
    /// <summary>Applies a field mapping to the object codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline (<*/>) (rest: SplitCodec<_, _->'Rest, _>) (fieldName, getter: 'T -> 'Value) = rfield fieldName getter rest

    /// <summary>Appends the first field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="f">An object initializer as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline (<!/>) f (fieldName, getter: 'T -> 'Value) = rfield fieldName getter (withFields f)

    /// <summary>Appends an optional field mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="rest">The other mappings.</param>
    /// <returns>The resulting object codec.</returns>
    let inline (<*/?>) (rest: SplitCodec<_, _ -> 'Rest, _>) (fieldName, getter: 'T -> 'Value option) = rfieldOpt fieldName getter rest

    /// <summary>Appends the first field (optional) mapping to the codec.</summary>
    /// <param name="fieldName">A string that will be used as key to the field.</param>
    /// <param name="getter">The field getter function.</param>
    /// <param name="f">An object initializer as a curried function.</param>
    /// <returns>The resulting object codec.</returns>
    let inline (<!/?>) f (fieldName, getter: 'T -> 'Value option) = rfieldOpt fieldName getter (withFields f)

    /// Tuple two values.
    let inline (^=) a b = (a, b)

    /// Gets a value from a Redis object
    let inline rgetFromListWith ofRedis (o: list<HashEntry>) key =
      match tryFindEntry key o with
      | Some value -> ofRedis value
      | _ -> Decode.Fail.propertyNotFound key (ofList o)

    /// Tries to get a value from a Redis object.
    /// Returns None if key is not present in the object.
    let inline rgetFromListOptWith ofRedis (o: list<HashEntry>) key =
      match tryFindEntry key o with
      | Some value -> ofRedis value |> map Some
      | _ -> Ok None

    let inline roptWith codec prop getter =
      {
          Decoder = ReaderT (fun (o: list<HashEntry>) -> rgetFromListOptWith (fst codec) o prop)
          Encoder = fun x -> Const (match getter x with Some (x: 'Value) -> [HashEntry (implicit prop, (snd codec) x)] | _ -> [])
      }

    /// Derives a concrete field codec for an optional field
    let inline ropt prop getter = roptWith redisValueCodec prop getter

    let inline rreqWith codec (prop: string) (getter: 'T -> 'Value option) =
      {
          Decoder = ReaderT (fun (o: list<HashEntry>) -> rgetFromListWith (fst codec) o prop)
          Encoder = fun x -> Const (match getter x with Some (x: 'Value) -> [HashEntry (implicit prop, (snd codec) x)] | _ -> [])
      }

    /// Derives a concrete field codec for a required field
    let inline rreq (name: string) (getter: 'T -> 'param option) = rreqWith redisValueCodec name getter

    let inline rchoice (codecs: seq<ConcreteCodec<'S, 'S, 't1, 't2>>) =
      let head, tail = Seq.head codecs, Seq.tail codecs
      foldBack (<|>) tail head