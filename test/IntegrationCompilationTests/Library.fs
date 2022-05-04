namespace IntegrationCompilationTests

module Startup =
    Fleece.Config.codecCacheEnabled <- true

module LegacyTests =

    type Person (name: string, age: int, children: Person list) =
        member __.Name = name
        member __.Age = age
        member __.Children = children

    open FSharpPlus
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
    open FSharpPlus
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
                and! children = jopt "Children" (fun x ->      x.Children)
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
    open FSharpPlus
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


module TestInterfaces =
    
    open Fleece
    
    type IVehicle =
        abstract member MaxSpeed : unit -> float
        inherit ICodecInterface<IVehicle>
    
    
    type Car = Car of Brand: string * MaxSpeed : float with
        interface IVehicle with
            member x.MaxSpeed () = let (Car (_, s)) = x in s
    
        static member ObjCodec () = codec {
                let! _        = jreq "type-Car" (fun _ -> Some ())
                and! brand    = jreq "brand"    (fun (Car (brand, _)) -> Some brand)
                and! maxSpeed = jreq "maxSpeed" (fun (Car (_, maxSpeed)) -> Some maxSpeed)
                return Car (Brand = brand, MaxSpeed = maxSpeed)
            }
            
    
    type Truck = { Brand: string; MaxSpeed : float; MaxLoad : float; Extra : (int * int * int * int * int * int * int * int) } with
        interface IVehicle with
            member x.MaxSpeed () = let { MaxSpeed = s } = x in s
    
        static member ObjCodec () = codec {
            let! _        = jreq "type-Truck" (fun _ -> Some ())
            and! brand    = jreq "brand"    (fun { Brand = x } -> Some x)
            and! maxSpeed = jreq "maxSpeed" (fun { MaxSpeed = x } -> Some x)
            and! maxLoad  = jreq "maxLoad"  (fun { MaxLoad = x } -> Some x)
            and! extra    = jreq "extra"    (fun { Extra = x} -> Some x)
            return { Brand = brand; MaxSpeed = maxSpeed; MaxLoad = maxLoad; Extra = extra }
        }

    type Garage = { Vehicle : IVehicle } with
        static member get_Codec () = ofObjCodec <| codec {
            let! v = jreq "Vehicle" (fun x -> Some x.Vehicle)
            return { Vehicle = v} }
    
    let car = Car (Brand = "Volvo", MaxSpeed = 120.0) :> IVehicle
    let truck =  { Brand = "Ford" ; MaxSpeed = 100.0; MaxLoad = 2500.0; Extra = (1, 2, 3, 4, 5, 6, 7, 8) } :> IVehicle
    let gcar =   { Vehicle = car }
    let gtruck = { Vehicle = truck }
    
    do ICodecInterface<IVehicle>.RegisterCodec<AdHocEncoding, Car> Car.ObjCodec

    let stjGCarJson = Fleece.SystemTextJson.Operators.toJsonText gcar
    let stjCarJson  = Fleece.SystemTextJson.Operators.toJsonText car
    let nsjCarJson  = Fleece.Newtonsoft.Operators.toJsonText car

    Assert.StringContains ("", "brand", nsjCarJson)
    Assert.StringContains ("", "brand", stjCarJson)
    Assert.StringContains ("", "brand", stjGCarJson)

    do ICodecInterface<IVehicle>.RegisterCodec<AdHocEncoding, Truck> Truck.ObjCodec

    let stjGTruckJson = Fleece.SystemTextJson.Operators.toJsonText gtruck    
    let stjTruckJson  = Fleece.SystemTextJson.Operators.toJsonText truck
    let nsjTruckJson  = Fleece.Newtonsoft.Operators.toJsonText truck

    Assert.StringContains ("", "brand", nsjTruckJson)
    Assert.StringContains ("", "brand", stjTruckJson)
    Assert.StringContains ("", "brand", stjGTruckJson)

    let xNsjCarJson: ParseResult<IVehicle> = Fleece.SystemJson.Operators.ofJsonText nsjCarJson
    let xSjCarJson : ParseResult<IVehicle> = Fleece.Newtonsoft.Operators.ofJsonText nsjCarJson
    let xStjCarJson: ParseResult<IVehicle> = Fleece.SystemTextJson.Operators.ofJsonText nsjCarJson

    Assert.Equal ("At least one decoding operation failed.", (Ok car, Ok car, Ok car), (xNsjCarJson, xSjCarJson, xStjCarJson) )


module TestCompilationPartiallyInferredTypes =
    open FSharpPlus
    open Fleece.SystemTextJson
    open Fleece
    
    type OpError = inherit ICodecInterface<OpError>
    type Subject = inherit ICodecInterface<Subject>
    
    type ConstructionError<'OpError when 'OpError :> OpError> = CError of 'OpError
    with
        static member inline get_Codec () = ofObjCodec (CError <!> jreq "ConstructionError" (function (CError x) -> Some x ))
    
        static member CastUnsafe (CError err: ConstructionError<#OpError>) : ConstructionError<'OpError> = CError (err |> box :?> 'OpError)
    
    let inline forIsomorphicType (toCodecFriendly: 'T -> 'CodecFriendly) (ofCodecFriendly: 'CodecFriendly -> 'T) =
        ((fun (t: obj) -> t :?> 'T |> toCodecFriendly |> toJsonText), ofJsonText<'CodecFriendly> >> Result.get >> ofCodecFriendly >> box)
    
    // Here Result is partially inferred without its generic types.
    // Using overload for error messages, catching struct for generics resolves early.
    // The following lines were failing to compile in 0.10.0-RC4 but subsequent changes seems to have fixed the problem somehow.
    let _ = forIsomorphicType
                (Result.map (fun x -> x :> Subject) >> Result.mapError ConstructionError<OpError>.CastUnsafe)
                (Result.map id                      >> Result.mapError ConstructionError<OpError>.CastUnsafe)
    ()


module TestIDictionaries =
    open FSharpPlus.Data
    open Fleece.Helpers

    let a1 = Fleece.SystemTextJson.Operators.toJsonText (Map.ofList ["One", 1; "Two", 2])
    let a2 = Fleece.SystemTextJson.Operators.toJsonText (Map.ofList ['a', 1; 'b', 2])

    let _: Result<Map<string, int>, _> = Fleece.SystemTextJson.Operators.ofJsonText a1
    let _: Result<Map<float , int>, _> = Fleece.SystemTextJson.Operators.ofJsonText a2

    Assert.Equal ("Map with string properties", "{\"One\":1,\"Two\":2}", a1)
    Assert.Equal ("Map with non-string properties", "[[\"a\",1],[\"b\",2]]", a2)

    let c1 = Fleece.SystemTextJson.Operators.toJsonText (Dictionary.ofSeq ["One", 1; "Two", 2])
    let c2 = Fleece.SystemTextJson.Operators.toJsonText (Dictionary.ofSeq ['a', 1; 'b', 2])

    let _: Result<Map<string, int>, _> = Fleece.SystemTextJson.Operators.ofJsonText c1
    let _: Result<Map<float , int>, _> = Fleece.SystemTextJson.Operators.ofJsonText c2

    Assert.Equal ("Dictionary with string properties", "{\"One\":1,\"Two\":2}", c1)
    Assert.Equal ("Dictionary with non-string properties", "[[\"a\",1],[\"b\",2]]", c2)

    let e1 = Fleece.SystemTextJson.Operators.toJsonText (Map.ofList ["One", 1; "Two", 2])
    let e2 = Fleece.SystemTextJson.Operators.toJsonText (Map.ofList ['a', 1; 'b', 2])

    let _: Result<NonEmptyMap<string, int>, _> = Fleece.SystemTextJson.Operators.ofJsonText e1
    let _: Result<NonEmptyMap<float , int>, _> = Fleece.SystemTextJson.Operators.ofJsonText e2

    Assert.Equal ("NonEmptyMap with string properties", "{\"One\":1,\"Two\":2}", e1)
    Assert.Equal ("NonEmptyMap with non-string properties", "[[\"a\",1],[\"b\",2]]", e2)


module TestGenerics =
    open Fleece
    
    let inline listOfSomething () : Codec<'Encoding, list<'t>> = defaultCodec
    let x: Fleece.SystemTextJson.Encoding = (listOfSomething () |> Codec.encode) [1]
    let y: Fleece.SystemTextJson.Encoding = (listOfSomething () |> Codec.encode) ['a']
    ()