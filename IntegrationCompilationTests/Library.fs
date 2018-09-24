namespace IntegrationCompilationTests
open FSharpPlus

type Person (name:string, age:int, children:Person list)= 
    member __.Name = name
    member __.Age = age
    member __.Children = children

open Fleece
open Fleece.SystemJson
open Fleece.SystemJson.Operators
open System.Json
type PersonSystemJson(name:string, age:int, children:PersonSystemJson list) = 
    inherit Person(name, age, children |> map (fun p->p :> Person))
    member __.Children = children

with
    static member Create name age children = PersonSystemJson(name,age,children)

    static member OfJson json =
        match json with
        | JObject o -> PersonSystemJson.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member ToJson (x: PersonSystemJson) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ] 

open FSharp.Data
open Fleece.FSharpData
open Fleece.FSharpData.Operators

type PersonFSharpData(name:string, age:int, children:PersonFSharpData list) = 
    inherit Person(name, age, children |> map (fun p->p:>Person))
    member __.Children = children

with
    static member Create name age children = PersonFSharpData(name,age,children)

    static member OfJson json =
        match json with
        | JObject o -> PersonFSharpData.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member ToJson (x:PersonFSharpData) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ] 

open Newtonsoft.Json
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators

type PersonNewtonsoft(name:string, age:int, children:PersonNewtonsoft list) = 
    inherit Person(name, age, children |> map (fun p->p:>Person))
    member __.Children = children

with
    static member Create name age children = PersonNewtonsoft(name,age,children)

    static member OfJson json =
        match json with
        | JObject o -> PersonNewtonsoft.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member ToJson (x:PersonNewtonsoft) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ] 





module Say =
    let hello name =
        printfn "Hello %s" name
