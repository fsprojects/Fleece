namespace IntegrationCompilationTests
open FSharpPlus


type Person (name: string, age: int, children: Person list) =
    member __.Name = name
    member __.Age = age
    member __.Children = children


open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators

type PersonFSharpData (name: string, age: int, children: PersonFSharpData list) =
    inherit Person (name, age, children |> map (fun p -> p :> Person))
    member __.Children = children
with
    static member Create name age children = PersonFSharpData (name, age, children)

    static member OfJson json =
        match json with
        | JObject o -> PersonFSharpData.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Decode.Fail.objExpected x

    static member ToJson (x:PersonFSharpData) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ]


open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators

type PersonNewtonsoft (name: string, age: int, children: PersonNewtonsoft list) =
    inherit Person (name, age, children |> map (fun p -> p :> Person))
    member __.Children = children
with
    static member Create name age children = PersonNewtonsoft (name, age, children)

    static member OfJson json =
        match json with
        | JObject o -> PersonNewtonsoft.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Decode.Fail.objExpected x

    static member ToJson (x: PersonNewtonsoft) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ]


open Fleece.SystemJson
open Fleece.SystemJson.Operators

type PersonSystemJson (name: string, age: int, children: PersonSystemJson list) =
    inherit Person (name, age, children |> map (fun p -> p :> Person))
    member __.Children = children
with
    static member Create name age children = PersonSystemJson (name, age, children)

    static member OfJson json =
        match json with
        | JObject o -> PersonSystemJson.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Decode.Fail.objExpected x

    static member ToJson (x: PersonSystemJson) =
        jobj [ 
            "name" .= x.Name
            "age" .= x.Age
            "children" .= x.Children
        ]


open Fleece.SystemTextJson
open Fuchu

module TestDifferentDecoderEncoderForEachJsonLibrary =
    open System

    type Gender =
        | Male = 1
        | Female = 2

    type Person = {
        Name: string
        Age: int
        Gender: Gender
        DoB: DateTime
        Children: Person list
    }

    with
        static member NsjToJson (x: Person) : NsjEncoding =
            Fleece.Newtonsoft.Operators.jobj [ 
                "Name"     .= x.Name
                "Age"      .= x.Age
                "Gender"   .= x.Gender
                "DoB"      .= x.DoB
                "Children" .= x.Children
            ]

        static member StjToJson (x: Person) : StjEncoding =
            Fleece.SystemTextJson.Operators.jobj [ 
                "name"     .= x.Name
                "age"      .= x.Age
                "gender"   .= x.Gender
                "dob"      .= x.DoB
                "children" .= x.Children
            ]


        static member Encode (x, r: byref<NsjEncoding>) = r <- Person.NsjToJson x
        static member Encode (x, r: byref<StjEncoding>) = r <- Person.StjToJson x

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime(1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime(1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime(1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }

    let personText1 = person |> Fleece.Newtonsoft.Main.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Main.toJson |> string    
    
    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "dob", personText2)

module TestDifferentCodecsForEachJsonLibrary =
    open System

    type Gender =
        | Male = 1
        | Female = 2
    
    type Person = {
        Name: string
        Age: int
        Gender: Gender
        DoB: DateTime
        Children: Person list
    }

    with
        static member NsjCodec : Codec<NsjEncoding, _> =
            codec {
                let! name     = req "Name"     (fun x -> Some x.Name)
                and! age      = req "Age"      (fun x -> Some x.Age)
                and! gender   = req "Gender"   (fun x -> Some x.Gender)
                and! dob      = req "DoB"      (fun x -> Some x.DoB)
                and! children = req "Children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member StjCodec : Codec<StjEncoding, _> =
            codec {
                let! name     = req "name"     (fun x -> Some x.Name)
                and! age      = req "age"      (fun x -> Some x.Age)
                and! gender   = req "gender"   (fun x -> Some x.Gender)
                and! dob      = req "dob"      (fun x -> Some x.DoB)
                and! children = req "children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member Codec (r: byref<Codec<NsjEncoding, _>>) = r <- Person.NsjCodec
        static member Codec (r: byref<Codec<StjEncoding, _>>) = r <- Person.StjCodec

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime(1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime(1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime(1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }
    
    let personText1 = person |> Fleece.Newtonsoft.Main.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Main.toJson |> string

    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "dob", personText2)