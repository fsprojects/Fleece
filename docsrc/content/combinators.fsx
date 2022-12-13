(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "nuget: System.Json, 4.7.1"
#r "nuget: FSharpPlus, 1.2.2"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.0/Fleece.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.0/Fleece.SystemJson.dll"


(**
```f#
#r "nuget: Fleece.SystemJson"
```

*)

open Fleece
open Fleece.SystemJson
open Fleece.SystemJson.Operators

(**

## Combinators

So far we've seen how Fleece is capable of encoding/decoding by deriving automatically a codec from static members in the type.

But for those cases where we don't have control over the types (extension members won't be taken into account) we can explicitly specify combinators.

To do so, a set of the available functions exists, ending with the `With` suffix, which accepts a combinator as first parameter:

*)


type Color = Red | Blue | White

type Car = {
    Id : string
    Color : Color
    Kms : int }

let colorDecoder = function
    | JString "red"   -> Decode.Success Red
    | JString "blue"  -> Decode.Success Blue
    | JString "white" -> Decode.Success White
    | JString  x as v -> Decode.Fail.invalidValue v ("Wrong color: " + x)
    | x               -> Decode.Fail.strExpected  x

let colorEncoder = function
    | Red   -> JString "red"
    | Blue  -> JString "blue"
    | White -> JString "white"

let colorCodec = colorDecoder <-> colorEncoder

let [<GeneralizableValue>]carCodec<'t> =
    codec {
        let! i = jreqWith Codecs.string "id"    (fun x -> Some x.Id)
        and! c = jreqWith colorCodec    "color" (fun x -> Some x.Color)
        and! k = jreqWith Codecs.int    "kms"   (fun x -> Some x.Kms)
        return { Id = i; Color = c; Kms = k }
    }
    |> Codec.compose (Codecs.propList Codecs.id)

let car = { Id = "xyz"; Color = Red; Kms = 0 }

let jsonCar = Codec.encode carCodec car
// val jsonCar : JsonValue = {"id": "xyz", "color": "red", "kms": 0}
