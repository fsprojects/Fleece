(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/System.Json.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/Fleece.SystemJson.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/FSharpPlus.dll"

open System.Json
open Fleece.SystemJson
open Fleece.SystemJson.Operators
#if FSHARPDATA
#r @"../../src/Fleece.FSharpData/bin/Release/netstandard2.1/FSharp.Data.dll"
#r @"../../src/Fleece.FSharpData/bin/Release/netstandard2.1/Fleece.FSharpData.dll"
#r @"../../src/Fleece.FSharpData/bin/Release/netstandard2.1/FSharpPlus.dll"

open FSharp.Data
open Fleece.FSharpData
open Fleece.FSharpData.Operators
#endif
(**
## ToJson and OfJson

In order to parse or encode instances into Json you can define static members

For example, given this data type:
*)


type Person = {
    Name: string
    Age: int
    Children: Person list
}

(**
You can map it to JSON like this:
*)


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

(**
And you can map it from JSON like this:
*)

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

let john : Person ParseResult = parseJson """{
    "name": "John",
    "age": 44,
    "children": [{
        "name": "Katy",
        "age": 5,
        "children": []
    }, {
        "name": "Johnny",
        "age": 7,
        "children": []
    }]
}"""

(**
Though it's much easier to do this in a monadic or applicative way. For example, using [FSharpPlus](https://github.com/fsprojects/FSharpPlus) (which is already a dependency of Fleece):
*)

open FSharpPlus

type PersonAp = {
    Name: string
    Age: int
    Children: PersonAp list
}

type PersonAp with
    static member Create name age children = { PersonAp.Name = name; Age = age; Children = children }

    static member OfJson json =
        match json with
        | JObject o -> PersonAp.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Decode.Fail.objExpected x

(**

Or monadically:
*)

type PersonM = {
    Name: string
    Age: int
    Children: PersonM list
}

type PersonM with
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
        | x -> Decode.Fail.objExpected x

(**
Or you can use the Choice monad/applicative in [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras) instead, if you prefer.

You can see more examples in the [EdmundsNet](https://github.com/mausch/EdmundsNet) project.
*)
