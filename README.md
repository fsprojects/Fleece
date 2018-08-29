Fleece
======

Fleece is a JSON mapper for F#. It simplifies mapping from [System.Json](http://bit.ly/1axIBoA)'s JsonValue onto your types, and mapping from your types onto JsonValue. It's also available for [FSharp.Data](http://fsharp.github.io/FSharp.Data/)'s and NewtonSoft's JSON types if you prefer it over System.Json.
Its design is strongly influenced by Haskell's [Aeson](http://hackage.haskell.org/package/aeson-0.7.0.0/docs/Data-Aeson.html). Like Aeson, Fleece is designed around two typeclasses (in [FSharpPlus](https://github.com/fsprojects/FSharpPlus) style) ToJson and OfJson.

### Download binaries

* [For System.Json](https://www.nuget.org/packages/Fleece/)
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
open System.Json
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

printfn "%s" (string (toJson p))
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
            | Success name, Success age, Success children -> 
                Success {
                    Person.Name = name
                    Age = age
                    Children = children
                }
            | x -> Failure (sprintf "Error parsing person: %A" x)
        | x -> Failure (sprintf "Expected person, found %A" x)
        
let john : Person ParseResult = parseJson """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""        
```

Though it's much easier to do this in a monadic or applicative way. For example, using [FSharpPlus](https://github.com/fsprojects/FSharpPlus) (which is already a dependency of Fleece):

```fsharp
open FSharpPlus

type Person with
    static member Create name age children = { Person.Name = name; Age = age; Children = children }

    static member OfJson json =
        match json with
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

```

Or monadically:


```fsharp
type Person with
    static member OfJson json =
        match json with
        | JObject o -> 
            monad {
                let! name = o .@ "name"
                let! age = o .@ "age"
                let! children = o .@ "children"
                return {
                    Person.Name = name
                    Age = age
                    Children = children
                }
            }
        | x -> Failure (sprintf "Expected person, found %A" x)
```

Or you can use the Choice monad/applicative in [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras) instead, if you prefer.

You can see more examples in the [EdmundsNet](https://github.com/mausch/EdmundsNet) project.


CODEC
=====

For types that deserialize to Json Objets, typically (but not limited to) records, you can alternatively use codecs and have a single method which maps between fields and values. 


```fsharp
// Example

type Person =
  { 
    name : string * string
    age : int option
    children: Person list
  } with
    static member JsonObjCodec =
        fun f l a c -> { name = (f, l); age = a; children = c }
        <!/> "firstName" ^= fun x -> fst x.name
        <*/> "lastName"  ^= fun x -> snd x.name
        <*/?> "age"      ^= fun x -> x.age   // For optional fields you can use the same operators but ending with '?' :
        <*/> "children"  ^= fun x -> x.children


let p = {name = ("John", "Doe"); age = None; children = [{name = ("Johnny", "Doe"); age = Some 21; children = []}]}
printfn "%s" (string (toJson p))

let john = parseJson<Person> """{"children": [{"children": [],"age": 21,"lastName": "Doe","firstName": "Johnny"}],"lastName": "Doe","firstName": "John"}"""
```

If you prefer you can write the same with functions:

```fsharp
// Example

type Person =
  { 
    name : string * string
    age : int option
    children: Person list
  } with
    static member JsonObjCodec =
        fun f l a c -> { name = (f, l); age = a; children = c }
		|> mapping
        |> jfield    "firstName" (fun x -> fst x.name)
        |> jfield    "lastName"  (fun x -> snd x.name)
        |> jfieldopt "age"       (fun x -> x.age)
        |> jfield    "children"  (fun x -> x.children)

```
