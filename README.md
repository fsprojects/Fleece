Fleece
======

Fleece is a JSON mapper for F#. It simplifies mapping from [System.Json](http://bit.ly/1axIBoA)'s JsonValue onto your types, and mapping from your types onto JsonValue.
Its design is strongly influenced by Haskell's [Aeson](http://hackage.haskell.org/package/aeson-0.7.0.0/docs/Data-Aeson.html). Like Aeson, Fleece is designed around two typeclasses (in [FsControl](https://github.com/gmpl/FsControl) style) ToJSON and FromJSON.

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
    static member instance (ToJSON, x: Person, _:JsonValue) = fun () ->
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ]

```

