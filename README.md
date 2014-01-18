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

