(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "nuget: System.Json"
#r "nuget: Fleece.SystemJson"
#r "nuget: FSharpPlus"

open Fleece.SystemJson
open Fleece.SystemJson.Operators

(**
## CODEC

For types that deserialize to Json Objets, typically (but not limited to) records, you can alternatively use codecs and have a single method which maps between fields and values.

*)

type Person = {
    name : string * string
    age : int option
    children: Person list }
    with
    static member JsonObjCodec =
        fun f l a c -> { name = (f, l); age = a; children = c }
        <!> jreq  "firstName" (Some << fun x -> fst x.name)
        <*> jreq  "lastName"  (Some << fun x -> snd x.name)
        <*> jopt  "age"       (fun x -> x.age) // Optional fields: use 'jopt'
        <*> jreq  "children"  (fun x -> Some x.children)


let p = {name = ("John", "Doe"); age = None; children = [{name = ("Johnny", "Doe"); age = Some 21; children = []}]}
//printfn "%s" (string (toJson p))

let john = parseJson<Person> """{
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
If you prefer you can write the same with functions:
*)

type PersonF = {
    name : string * string
    age : int option
    children: PersonF list }
    with
    static member JsonObjCodec =
        fun f l a c -> { name = (f, l); age = a; children = c }
        |> withFields
        |> jfield    "firstName" (fun x -> fst x.name)
        |> jfield    "lastName"  (fun x -> snd x.name)
        |> jfieldOpt "age"       (fun x -> x.age)
        |> jfieldWith jsonValueCodec "children"  (fun x -> x.children)

(**
Both approaches build a codec from the same pieces:

- A constructor function that builds a new record from deserialized pieces
- A sequence of field specifications with `jfield/jfieldOpt` or `jreq/jot`.
  These specs take a field name and a function for getting that fields value from a record instance.

Discriminated unions can be modeled with alternatives:
*)

type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with
        static member JsonObjCodec =
            Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
            <|> ( Circle <!> jreq "radius" (function Circle x -> Some x | _ -> None) )
            <|> ( Prism <!> jreq "prism"   (function Prism (x, y, z) -> Some (x, y, z) | _ -> None) )
(**
or using the jchoice combinator:
*)

type ShapeC =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with
        static member JsonObjCodec =
            jchoice
                [
                    Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
                    Circle    <!> jreq "radius"    (function Circle x -> Some x | _ -> None)
                    Prism     <!> jreq "prism"     (function Prism (x, y, z) -> Some (x, y, z) | _ -> None)
                ]

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
        static member JsonObjCodec =
            jchoice
                [
                    (fun () -> North) <!> jreq "north" (function North -> Some () | _ -> None)
                    (fun () -> South) <!> jreq "south" (function South -> Some () | _ -> None)
                    (fun () -> East) <!> jreq "east" (function East -> Some () | _ -> None)
                    (fun () -> West) <!> jreq "west" (function West -> Some () | _ -> None)
                ]


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
        "radius": "37.8"
    },
    {
        "type": "prism",
        "width": [10.0, 23.0],
        "height": 9.10
    }
]
"""

open FSharpPlus.Operators

type ShapeD =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with
        static member JsonObjCodec =
            jchoice
                [
                    Rectangle <!> jreq "type" (function Rectangle (_) -> Some "rectangle" | _ -> None)
                    <*> jreq "width" (function Rectangle(w, _) -> Some w | _ -> None)
                    <*> jreq "length" (function Rectangle(_, l) -> Some l | _ -> None)

                    Circle <!> jreq "type" (function Circle (_) -> Some "circle" | _ -> None)
                    <*> jreq "radius" (function Circle (r) -> Some r | _ -> None)

                    Prism <!> jreq "type" (function Prism (_) -> Some "prism" | _ -> None)
                    <*> jreq "width" (function Prism (x, y, _) -> Some (x, y) | _ -> None)
                    <*> jreq "height" (function Prism (_, _, h) -> Some h | _ -> None)
                ]

(**
We can manipulate codecs by using functions in the Codec module. Here's an example:
*)
open System.Text
let pf : PersonF= {name = ("John", "Doe"); age = None; children = [{name = ("Johnny", "Doe"); age = Some 21; children = []}]}

let personBytesCodec =
    let getString (bytes:byte array) = Encoding.UTF8.GetString bytes
    PersonF.JsonObjCodec
    |> Codec.compose jsonObjToValueCodec    // this is the codec that fills the gap to/from JsonValue
    |> Codec.compose jsonValueToTextCodec   // this is a codec between JsonValue and JsonText
    |> Codec.invmap getString Encoding.UTF8.GetBytes    // This is a pair of of isomorphic functions

let bytePerson = Codec.encode personBytesCodec pf
// val bytePerson : byte [] = [|123uy; 13uy; 10uy; 32uy; 32uy; ... |]
let p' = Codec.decode personBytesCodec bytePerson

(**
While if the type of codec is concrete then we need to convert it to before composing it
*)

let personBytesCodec2 =
    let getString (bytes:byte array) = Encoding.UTF8.GetString bytes
    Person.JsonObjCodec
    |> Codec.ofConcrete
    |> Codec.compose jsonObjToValueCodec    // this is the codec that fills the gap to/from JsonValue
    |> Codec.compose jsonValueToTextCodec   // this is a codec between JsonValue and JsonText
    |> Codec.invmap getString Encoding.UTF8.GetBytes    // This is a pair of of isomorphic functions
