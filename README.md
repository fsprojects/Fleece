Fleece
======

Fleece is a JSON mapper for F#. It simplifies mapping from a Json library's JsonValue onto your types, and mapping from your types onto JsonValue. 

The Json library could be [System.Json](http://bit.ly/1axIBoA), [System.Text.Json](https://docs.microsoft.com/en-us/dotnet/api/system.text.json), [FSharp.Data](http://fsharp.github.io/FSharp.Data/)'s or [NewtonSoft's Json.NET](https://www.newtonsoft.com/json).

Its design is strongly influenced by Haskell's [Aeson](http://hackage.haskell.org/package/aeson-0.7.0.0/docs/Data-Aeson.html). Like Aeson, Fleece is designed around two typeclasses (in [FSharpPlus](https://github.com/fsprojects/FSharpPlus) style) ToJson and OfJson.

### Download binaries
* [Fleece core](https://www.nuget.org/packages/Fleece)
* [For System.Json](https://www.nuget.org/packages/Fleece.SystemJson/)
* [For System.Text.Json](https://www.nuget.org/packages/Fleece.SystemTextJson/)
* [For FSharp.Data](https://www.nuget.org/packages/Fleece.FSharpData/)
* [For NewtonSoft](https://www.nuget.org/packages/Fleece.NewtonSoftJson/)

### Example

For example, given this data type:

```fsharp
type Person = {
    Name: string
    Age: int
    Children: Person list
}
```

You can map it to JSON like this:

```fsharp
open Fleece
open Fleece.Operators

type Person with
    static member ToJson (x: Person) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ]

let p = 
    { Person.Name = "John"
      Age = 44
      Children = 
      [
        { Person.Name = "Katy"
          Age = 5
          Children = [] }
        { Person.Name = "Johnny"
          Age = 7
          Children = [] }
      ] }

// Test with System.Text.Json

open Fleece.SystemTextJson
printfn "%s" (toJsonText p)
```

And you can map it from JSON like this:

```fsharp
type Person with
    static member OfJson json =
        match json with
        | JObject o ->
            let name = o .@ "name"
            let age = o .@ "age"
            let children = o .@ "children"
            match name, age, children with
            | Decode.Success name, Decode.Success age, Decode.Success children ->
                Decode.Success {
                    Person.Name = name
                    Age = age
                    Children = children
                }
            | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
        | x -> Decode.Fail.objExpected x
        
let john : Person ParseResult = ofJsonText """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""
```

Though it's much easier to do this in a monadic or applicative way. For example, using [FSharpPlus](https://github.com/fsprojects/FSharpPlus) (which is already a dependency of Fleece):

```fsharp
open FSharpPlus

type Person with
    static member Create name age children = { Person.Name = name; Age = age; Children = children }

    static member OfJson json =
        match json with
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Decode.Fail.objExpected x

```

or with applicatives:


```fsharp

open FSharpPlus

type Person with
    static member OfJson json =
        match json with
        | JObject o -> 
            monad {
                let! name = o .@ "name"
                and! age = o .@ "age"
                and! children = o .@ "children"
                return {
                    Person.Name = name
                    Age = age
                    Children = children
                }
            }
        | x -> Decode.Fail.objExpected x
```

Or you can use the Choice monad/applicative in [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras) instead, if you prefer.

You can see more examples in the [EdmundsNet](https://github.com/mausch/EdmundsNet) project.


### CODEC

For types that deserialize to Json Objets, typically (but not limited to) records, you can alternatively use codecs and have a single method which maps between fields and values. 


```fsharp

type Person = { 
    name : string * string
    age : int option
    children: Person list } 
    with
    static member get_Codec () =
        fun f l a c -> { name = (f, l); age = a; children = c }
        <!> jreq "firstName" (Some << fun x -> fst x.name)
        <*> jreq "lastName"  (Some << fun x -> snd x.name)
        <*> jopt "age"       (fun x -> x.age) // Optional fields can use 'jopt'
        <*> jreq "children"  (fun x -> Some x.children)
        |> ofObjCodec
        
let john: Person ParseResult = ofJsonText """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""
```


Discriminated unions can be modeled with alternatives:
```fsharp
type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with 
        static member get_Codec () =
            (Rectangle  <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None))
            <|> (Circle <!> jreq "radius"    (function Circle x -> Some x | _ -> None))
            <|> (Prism  <!> jreq "prism"     (function Prism (x, y, z) -> Some (x, y, z) | _ -> None))
            |> ofObjCodec
```


or using the jchoice combinator:
```fsharp
type Shape with
        static member JsonObjCodec =
            jchoice
                [
                    Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
                    Circle    <!> jreq "radius"    (function Circle x -> Some x | _ -> None)
                    Prism     <!> jreq "prism"     (function Prism (x, y, z) -> Some (x, y, z) | _ -> None)
                ]
             |> ofObjCodec

```

But codecs for both types can easily be written with the codec computation expressions

```fsharp

type Person = { 
    name : string * string
    age : int option
    children: Person list } 
    with
        static member get_Codec () =
            codec {
                let! f = jreq "firstName" (Some << fun x -> fst x.name)
                and! l = jreq "lastName"  (Some << fun x -> snd x.name)
                and! a = jopt "age"       (fun x -> x.age) // Optional fields can use 'jopt'
                and! c = jreq "children"  (fun x -> Some x.children)
                return { name = (f, l); age = a; children = c } }
            |> ofObjCodec
        

type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    with
        static member get_Codec () =
            codec {
                Rectangle <!> jreq "rectangle" (function Rectangle (x, y) -> Some (x, y) | _ -> None)
                Circle    <!> jreq "radius"    (function Circle x -> Some x | _ -> None)
                Prism     <!> jreq "prism"     (function Prism (x, y, z) -> Some (x, y, z) | _ -> None)
            }
            |> ofObjCodec

```


What's happening here is that we're getting a Codec to/from a Json Object (not neccesarily a JsonValue) which Fleece is able to take it and fill the gap by composing it with a codec from JsonObject to/from JsonValue.

We can also do that by hand, we can manipulate codecs by using functions in the Codec module. Here's an example:

```fsharp
open System.Text
open Fleece.SystemTextJson.Operators

type Person = { 
    name : string * string
    age : int option
    children: Person list } 
    with
        static member JsonObjCodec: Codec<PropertyList<Fleece.SystemTextJson.Encoding>, Person> = codec {
            let! f = jreq "firstName" (Some << fun x -> fst x.name)
            and! l = jreq "lastName"  (Some << fun x -> snd x.name)
            and! a = jopt "age"       (fun x -> x.age) // Optional fields can use 'jopt'
            and! c = jreq "children"  (fun x -> Some x.children)
            return { name = (f, l); age = a; children = c } }

let personBytesCodec =
    Person.JsonObjCodec
    |> Codec.compose jsonObjToValueCodec    // this is the codec that fills the gap to/from JsonValue
    |> Codec.compose jsonValueToTextCodec   // this is a codec between JsonValue and JsonText
    |> Codec.invmap (Encoding.UTF8.GetString: byte [] -> string) Encoding.UTF8.GetBytes    // This is a pair of of isomorphic functions

let p = { name = "John", "Smith"; age = Some 42; children = [] }


let bytePerson = Codec.encode personBytesCodec p
// val bytePerson : byte [] = [|123uy; 34uy; 102uy; 105uy; 114uy; 115uy; ... |]
let p' = Codec.decode personBytesCodec bytePerson
```

### Combinators

So far we've seen how Fleece is capable of encoding/decoding by deriving automatically a codec from static members in the type.

But for those cases where we don't have control over the types (extension members won't be taken into account) we can explicitly specify combinators.

To do so, a set of the available functions exists, ending with the `With` suffix, which accepts a combinator as first parameter:

```fsharp
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

let colorCodec () = colorDecoder <-> colorEncoder
    
let carCodec () =
    fun i c k -> { Id = i; Color = c; Kms = k }
    |> withFields
    |> jfieldWith Codecs.string "id"    (fun x -> x.Id)
    |> jfieldWith (colorCodec ())    "color" (fun x -> x.Color)
    |> jfieldWith Codecs.int    "kms"   (fun x -> x.Kms)
    |> Codec.compose (Codecs.propList Codecs.id)

let car = { Id = "xyz"; Color = Red; Kms = 0 }

let jsonCar : Fleece.SystemTextJson.Encoding = Codec.encode (carCodec ()) car
// val jsonCar: SystemTextJson.Encoding = {"id":"xyz","color":"red","kms":0}
```

## Maintainer(s)

- [@mausch](https://github.com/mausch)
- [@gusty](https://github.com/gusty)
- [@wallymathieu](https://github.com/wallymathieu)
