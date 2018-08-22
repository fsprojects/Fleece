namespace IntegrationCompilationTests
open FSharp.Data
open Fleece
open Fleece.Operators
open System.Json
open Newtonsoft.Json
open FSharpPlus

type Person = {
    Name: string
    Age: int
    Children: Person list
}

type Person with
    static member Create name age children = { Person.Name = name; Age = age; Children = children }

    static member OfJson json = 
        match json with
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member ToJson (x: Person) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ] 

type Attribute = {
    Name: string
    Value: string
}


module Say =
    let hello name =
        printfn "Hello %s" name
