(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "nuget: System.Json, 4.7.1"
#r "nuget: FSharpPlus, 1.2.2"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.0/Fleece.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.0/Fleece.SystemJson.dll"

(**

## CODEC

```f#
#r "nuget: Fleece.SystemJson"
```
*)

open Fleece
open Fleece.SystemJson
open Fleece.SystemJson.Operators

(**
For types that deserialize to Json Objets, typically (but not limited to) records, you can alternatively use codecs and have a single method which maps between fields and values.
*)

type Person = {
    name : string * string
    age : int option
    children: Person list }
    with
    static member JsonObjCodec =
        fun f l a c -> { name = (f, l); age = a; children = c }
        <!> jreq  "firstName" (fun x -> Some (fst x.name))
        <*> jreq  "lastName"  (fun x -> Some (snd x.name))
        <*> jopt  "age"       (fun x -> x.age) // Optional fields: use 'jopt'
        <*> jreq  "children"  (fun x -> Some x.children)


let p = {name = ("John", "Doe"); age = None; children = [{name = ("Johnny", "Doe"); age = Some 21; children = []}]}
//printfn "%s" (string (toJson p))

let john = ofJsonText<Person> """{
    "children": [{
        "children": [],
        "age": 21,
        "lastName": "Doe",
        "firstName": "Johnny"
    }],
    "lastName": "Doe",
    "firstName": "John"
}"""

(**
If you prefer you can write the same with a codec computation expression:
*)

type PersonF = {
    name : string * string
    age : int option
    children: PersonF list }
    with
    static member JsonObjCodec = codec {
        let! f = jreq "firstName" (fun x -> Some (fst x.name))
        and! l = jreq "lastName"  (fun x -> Some (snd x.name))
        and! a = jopt "age"       (fun x -> x.age)
        and! c = jreq "children"  (fun x -> Some x.children)
        return { name = (f, l); age = a; children = c }
    }

(**
Both approaches build a codec from the same pieces:

- A constructor function that builds a new record from deserialized pieces
- A sequence of field specifications with `jfield/jfieldOpt` or `jreq/jot`.
  These specs take a field name and a function for getting that fields value from a record instance.

Discriminated unions can be modeled with alternatives:
*)

type Shape =
    | Rectangle of width  : float * length : float
    | Circle    of radius : float
    | Prism     of width  : float * float * height : float
    with
        static member JsonObjCodec =
            Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
            <|> ( Circle <!> jreq "radius" (function Circle x -> Some x | _ -> None) )
            <|> ( Prism <!> jreq "prism"   (function Prism (x, y, z) -> Some (x, y, z) | _ -> None) )
(**
or using the codec computation expression:
*)

type ShapeC =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with
        static member JsonObjCodec = codec {
            Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
            Circle    <!> jreq "radius"    (function Circle x -> Some x | _ -> None)
            Prism     <!> jreq "prism"     (function Prism (x, y, z) -> Some (x, y, z) | _ -> None)
        }

(**
What's happening here is that we're getting a Codec to/from a Json Object (not neccesarily a JsonValue) which Fleece is able to take it and fill the gap by composing it with a codec from JsonObject to/from JsonValue.

For DUs that carry no data, a function is still necessary:
*)

type CompassDirection =
    | North
    | East
    | South
    | West
    with
        static member JsonObjCodec = codec {
            (fun () -> North) <!> jreq "north" (function North -> Some () | _ -> None)
            (fun () -> South) <!> jreq "south" (function South -> Some () | _ -> None)
            (fun () -> East)  <!> jreq "east"  (function East  -> Some () | _ -> None)
            (fun () -> West)  <!> jreq "west"  (function West  -> Some () | _ -> None)
        }


(**
A common way to represent algebraic data types in JSON is to use a type tag.
For example:
**)

let someShapes = """
[
    {
        "type": "rectangle",
        "width": 8.8,
        "length": 12.0
    },
    {
        "type": "circle",
        "radius": 37.8
    },
    {
        "type": "prism",
        "width": [10.0, 23.0],
        "height": 9.10
    }
]
"""

open FSharpPlus
open FSharpPlus.Operators


type ShapeD =
    | Rectangle of width  : float * length : float
    | Circle    of radius : float
    | Prism     of width  : float * float * height : float
    with
        static member JsonObjCodec =
            /// Derives a field codec for a required field and value
            let inline jreqValue prop value codec =
                let matchPropValue o =
                     match IReadOnlyDictionary.tryGetValue prop o with
                     | Some a when ofJson a = Ok value -> Ok o
                     | Some a -> Decode.Fail.invalidValue a value
                     | None   -> Decode.Fail.propertyNotFound prop o
                codec
                |> Codec.compose (
                    matchPropValue <->
                    fun (encoded: PropertyList<Encoding>) ->
                        if encoded.Count = 0 then encoded // we have not encoded anything so no need to add property and value 
                        else PropertyList [|prop, toJson value|] ++ encoded
                    )

            jchoice
                [
                    fun w l -> Rectangle (w, l)
                    <!> jreq "width"  (function Rectangle(w, _) -> Some w | _ -> None)
                    <*> jreq "length" (function Rectangle(_, l) -> Some l | _ -> None)
                    |> jreqValue "type" "rectangle"

                    Circle
                    <!> jreq "radius" (function Circle r -> Some r | _ -> None)
                    |> jreqValue "type" "circle"

                    fun (w, w2) h -> Prism (w, w2, h)
                    <!> jreq "width"  (function Prism (x, y, _) -> Some (x, y) | _ -> None)
                    <*> jreq "height" (function Prism (_, _, h) -> Some h      | _ -> None)
                    |> jreqValue "type" "prism"
                ]

let parsedShapedD = ofJsonText<ShapeD list> someShapes

(**
We can manipulate codecs by using functions in the Codec module. Here's an example:
*)
open System.Text
let pf : PersonF= {name = ("John", "Doe"); age = None; children = [{name = ("Johnny", "Doe"); age = Some 21; children = []}]}

let personBytesCodec =
    let getString (bytes:byte array) = Encoding.UTF8.GetString bytes
    PersonF.JsonObjCodec
    |> Codec.compose jsonObjToValueCodec               // this is the codec that fills the gap to/from JsonValue
    |> Codec.compose jsonValueToTextCodec              // this is a codec between JsonValue and JsonText
    |> Codec.invmap getString Encoding.UTF8.GetBytes   // This is a pair of of isomorphic functions

let bytePerson = Codec.encode personBytesCodec pf
// val bytePerson : byte [] = [|123uy; 13uy; 10uy; 32uy; 32uy; ... |]
let p' = Codec.decode personBytesCodec bytePerson