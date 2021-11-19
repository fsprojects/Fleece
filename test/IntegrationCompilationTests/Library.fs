namespace IntegrationCompilationTests
open FSharpPlus

module LegacyTests =

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


open Fuchu

module TestSingleDecoderEncoderForAllJsonLibrary =

    open System
    open Fleece

    type Gender =
        | Male = 1
        | Female = 2

    type Person = {
        Name: string
        Age: int
        Gender: Gender
        DoB: DateTime
        Children: Person list
    } with
        static member ToJson (x: Person) =
            jobj [ 
                "name"     .= x.Name
                "age"      .= x.Age
                "gender"   .= x.Gender
                "dob"      .= x.DoB
                "children" .= x.Children
            ]

        static member OfJson (json: 'Encoding) =
            match json with
            | JObject o ->
                let name     = o .@ "name"
                let age      = o .@ "age"
                let dob      = o .@ "dob"
                let gender   = o .@ "gender"
                let children = o .@ "children"
                match name, age, dob, gender, children with
                | Decode.Success name, Decode.Success age, Decode.Success dob, Decode.Success gender, Decode.Success children ->
                    Decode.Success {
                        Name     = name
                        Age      = age
                        DoB      = dob
                        Gender   = gender
                        Children = children
                    }
                | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
            | x -> Decode.Fail.objExpected x

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }

    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson     |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    let person1: Person = personText1 |> Fleece.Newtonsoft.Operators.ofJsonText     |> Result.get
    let person2: Person = personText2 |> Fleece.SystemTextJson.Operators.ofJsonText |> Result.get

    Assert.StringContains ("", "dob", personText1)
    Assert.StringContains ("", "dob", personText2)


module TestSingleCodecForAllJsonLibrary =

    open System
    open Fleece

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
        static member get_Codec () : Codec<'Encoding, _> =
            codec {
                let! name     = jreq "Name"     (fun x -> Some x.Name)
                and! age      = jreq "Age"      (fun x -> Some x.Age)
                and! gender   = jreq "Gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "DoB"      (fun x -> Some x.DoB)
                and! children = jreq "Children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }
    
    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "DoB", personText2)


module TestDifferentDecoderEncoderForEachJsonLibrary =

    open System
    open Fleece
    open Fleece.Newtonsoft
    open Fleece.SystemTextJson

    type Gender =
        | Male = 1
        | Female = 2

    type Person = {
        Name: string
        Age: int
        Gender: Gender
        DoB: DateTime
        Children: Person list
    } with
        static member NsjToJson (x: Person) : Newtonsoft.Encoding =
            jobj [ 
                "Name"     .= x.Name
                "Age"      .= x.Age
                "Gender"   .= x.Gender
                "DoB"      .= x.DoB
                "Children" .= x.Children
            ]

        static member StjToJson (x: Person) : SystemTextJson.Encoding =
            jobj [ 
                "name"     .= x.Name
                "age"      .= x.Age
                "gender"   .= x.Gender
                "dob"      .= x.DoB
                "children" .= x.Children
            ]

        static member Encode (x, r: byref<Newtonsoft.Encoding>) = r <- Person.NsjToJson x
        static member Encode (x, r: byref<SystemTextJson.Encoding>) = r <- Person.StjToJson x

    type Person with
        static member OfJson (json: Newtonsoft.Encoding) =
            match json with
            | JObject o ->
                let name     = o .@ "Name"
                let age      = o .@ "Age"
                let gender   = o .@ "Gender"
                let dob      = o .@ "DoB"
                let children = o .@ "Children"
                match name, age, gender, dob, children with
                | Decode.Success name, Decode.Success age, Decode.Success gender, Decode.Success dob, Decode.Success children ->
                    Decode.Success {
                        Name     = name
                        Age      = age
                        Gender   = gender
                        DoB      = dob
                        Children = children
                    }
                | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
            | x -> Decode.Fail.objExpected x

        static member OfJson (json: SystemTextJson.Encoding) =
            match json with
            | JObject o ->
                let name     = o .@ "name"
                let age      = o .@ "age"
                let gender   = o .@ "gender"
                let dob      = o .@ "dob"
                let children = o .@ "children"
                match name, age, gender, dob, children with
                | Decode.Success name, Decode.Success age, Decode.Success gender, Decode.Success dob, Decode.Success children ->
                    Decode.Success {
                        Name     = name
                        Age      = age
                        Gender   = gender
                        DoB      = dob
                        Children = children
                    }
                | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
            | x -> Decode.Fail.objExpected x

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }

    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson     |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    let person1: Person = personText1 |> Fleece.Newtonsoft.Operators.ofJsonText     |> Result.get
    let person2: Person = personText2 |> Fleece.SystemTextJson.Operators.ofJsonText |> Result.get
    
    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "dob", personText2)


module TestDifferentCodecsForEachJsonLibrary =

    open System
    open Fleece
    open Fleece.Newtonsoft
    open Fleece.SystemTextJson

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
        static member NsjCodec : Codec<Newtonsoft.Encoding, _> =
            codec {
                let! name     = jreq "Name"     (fun x -> Some x.Name)
                and! age      = jreq "Age"      (fun x -> Some x.Age)
                and! gender   = jreq "Gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "DoB"      (fun x -> Some x.DoB)
                and! children = jreq "Children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member StjCodec : Codec<SystemTextJson.Encoding, _> =
            codec {
                let! name     = jreq "name"     (fun x -> Some x.Name)
                and! age      = jreq "age"      (fun x -> Some x.Age)
                and! gender   = jreq "gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "dob"      (fun x -> Some x.DoB)
                and! children = jreq "children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member Codec (r: byref<Codec<Newtonsoft.Encoding, _>>) = r <- Person.NsjCodec
        static member Codec (r: byref<Codec<SystemTextJson.Encoding, _>>) = r <- Person.StjCodec

    let person =
        { Person.Name = "John"
          Age = 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }
    
    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "dob", personText2)


module TestMixedCases =

    open System
    open Fleece

    type Age = Age of int with
        static member ToJson (Age x) = toEncoding x
        static member OfJson json = ofEncoding json |> Result.map Age

    type Gender =
        | Male = 1
        | Female = 2
    
    type Person = {
        Name: string
        Age: Age
        Gender: Gender
        DoB: DateTime
        Children: Person list
    }

    with
        static member get_Codec () : Codec<'Encoding, _> =
            codec {
                let! name     = jreq "Name"     (fun x -> Some x.Name)
                and! age      = jreq "Age"      (fun x -> Some x.Age)
                and! gender   = jreq "Gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "DoB"      (fun x -> Some x.DoB)
                and! children = jreq "Children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

    let person =
        { Person.Name = "John"
          Age = Age 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = Age 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = Age 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }
    
    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "DoB", personText2)


module TestDifferentCodecsForEachJsonLibraryMixedCases =

    open System
    open Fleece

    type Age = Age of int with
        static member Encode (Age x, r: byref<_>) = r <- Fleece.Newtonsoft.Operators.toJson x
        static member OfJson json = Fleece.Newtonsoft.Operators.ofJson json |> Result.map Age
        static member Encode (Age x, r: byref<_>) = r <- Fleece.SystemTextJson.Operators.toJson x
        static member OfJson json = Fleece.SystemTextJson.Operators.ofJson json |> Result.map Age

    type Gender =
        | Male = 1
        | Female = 2
    
    type Person = {
        Name: string
        Age: Age
        Gender: Gender
        DoB: DateTime
        Children: Person list
    }

    with
        static member NsjCodec : Codec<Newtonsoft.Encoding, _> =
            codec {
                let! name     = jreq "Name"     (fun x -> Some x.Name)
                and! age      = jreq "Age"      (fun x -> Some x.Age)
                and! gender   = jreq "Gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "DoB"      (fun x -> Some x.DoB)
                and! children = jreq "Children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member StjCodec : Codec<SystemTextJson.Encoding, _> =
            codec {
                let! name     = jreq "name"     (fun x -> Some x.Name)
                and! age      = jreq "age"      (fun x -> Some x.Age)
                and! gender   = jreq "gender"   (fun x -> Some x.Gender)
                and! dob      = jreq "dob"      (fun x -> Some x.DoB)
                and! children = jreq "children" (fun x -> Some x.Children)
                return { Name = name; Age = age; Gender = gender; DoB= dob; Children = children }
            } |> ofObjCodec

        static member Codec (r: byref<Codec<Newtonsoft.Encoding, _>>) = r <- Person.NsjCodec
        static member Codec (r: byref<Codec<SystemTextJson.Encoding, _>>) = r <- Person.StjCodec

    let person =
        { Person.Name = "John"
          Age = Age 44
          DoB = DateTime (1975, 01, 01)
          Gender = Gender.Male
          Children = 
          [
            { Person.Name = "Katy"
              Age = Age 5
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Female
              Children = [] }
            { Person.Name = "Johnny"
              Age = Age 7
              DoB = DateTime (1975, 01, 01)
              Gender = Gender.Male
              Children = [] }
          ]
          }
    
    let personText1 = person |> Fleece.Newtonsoft.Operators.toJson |> string
    let personText2 = person |> Fleece.SystemTextJson.Operators.toJson |> string

    Assert.StringContains ("", "DoB", personText1)
    Assert.StringContains ("", "dob", personText2)