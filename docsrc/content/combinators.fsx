(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/System.Json.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/Fleece.SystemJson.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/FSharpPlus.dll"

open Fleece.SystemJson

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

let colorCodec = colorDecoder, colorEncoder

let [<GeneralizableValue>]carCodec<'t> =
    fun i c k -> { Id = i; Color = c; Kms = k }
    |> withFields
    |> jfieldWith JsonCodec.string "id"    (fun x -> x.Id)
    |> jfieldWith colorCodec       "color" (fun x -> x.Color)
    |> jfieldWith JsonCodec.int    "kms"   (fun x -> x.Kms)
    |> Codec.compose jsonObjToValueCodec

let car = { Id = "xyz"; Color = Red; Kms = 0 }

let jsonCar = Codec.encode carCodec car
// val jsonCar : JsonValue = {"id": "xyz", "color": "red", "kms": 0}
