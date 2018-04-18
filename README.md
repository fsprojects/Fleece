Fleece
======

Fleece is a JSON mapper for F#. It simplifies mapping from [System.Json](http://bit.ly/1axIBoA)'s JsonValue onto your types, and mapping from your types onto JsonValue. It's also available for [FSharp.Data](http://fsharp.github.io/FSharp.Data/)'s JSON types if you prefer it over System.Json.
Its design is strongly influenced by Haskell's [Aeson](http://hackage.haskell.org/package/aeson-0.7.0.0/docs/Data-Aeson.html). Like Aeson, Fleece is designed around two typeclasses (in [FsControl](https://github.com/gmpl/FsControl) style) ToJSON and FromJSON.

### Download binaries

* [For FSharp.Data](https://www.nuget.org/packages/Fleece.FSharpData/)
* [For System.Json](https://www.nuget.org/packages/Fleece/)

###Example

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
    static member ToJSON (x: Person) =
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

printfn "%s" ((toJSON p).ToString())
```

And you can map it from JSON like this:

```fsharp
type Person with
    static member FromJSON json =
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
        
let john : Person ParseResult = parseJSON """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""        
```

Though it's much easier to do this in a monadic or applicative way. For example, using [FSharpPlus](https://github.com/gmpl/FSharpPlus) (which is already a dependency of Fleece):

```fsharp
open FSharpPlus

type Person with
    static member Create name age children = { Person.Name = name; Age = age; Children = children }

    static member FromJSON json =
        match json with
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

```

Or monadically:


```fsharp
type Person with
    static member FromJSON json =
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

Or you can use the Choice monad/applicative in [FSharpx.Core](https://github.com/fsprojects/fsharpx) instead, if you prefer.

You can see more examples in the [EdmundsNet](https://github.com/mausch/EdmundsNet) project.
