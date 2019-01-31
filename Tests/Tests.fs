module Tests.Tests
open System
open System.Text
open System.Collections.Generic
open System.Linq
open Fuchu
open FSharpPlus

#if FSHARPDATA
open FSharp.Data
open Fleece.FSharpData.Helpers
open Fleece.FSharpData
open Fleece.FSharpData.Operators
#endif
#if SYSTEMJSON
open Fleece.SystemJson.Helpers
open Fleece.SystemJson
open Fleece.SystemJson.Operators
open System.Json
#endif
#if NEWTONSOFT
open Newtonsoft.Json
open Fleece.Newtonsoft.Helpers
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators
#endif
let (|Success|Failure|) =
    function
    | Ok    x -> Success x
    | Error x -> Failure x
#nowarn "0686"

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
        | x -> Decode.Fail.objExpected x

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

type Attribute with
    static member Create name value = { Attribute.Name = name; Value = value }

    static member FromJSON (_: Attribute) =
        function
        | JObject o -> 
            monad {
                let! name = o .@ "name"
                if name = null then 
                    return! Decode.Fail.nullString
                else
                    let! value = o .@ "value"
                    return {
                        Attribute.Name = name
                        Value = value
                    }
            }
        | x -> Decode.Fail.objExpected x

    static member ToJson (x: Attribute) =
        jobj [ "name" .= x.Name; "value" .= x.Value ]

type Item = {
    Id: int
    Brand: string
    Availability: string option
}

type Item with
    static member JsonObjCodec =
        fun id brand availability -> { Item.Id = id; Brand = brand; Availability = availability }
        <!> jreq  "id"          (fun x -> Some x.Id     )
        <*> jreq  "brand"       (fun x -> Some x.Brand  )
        <*> jopt "availability" (fun x -> x.Availability)
        |> Codec.ofConcrete

type NestedItem = NestedItem of Item

type NestedItem with
    static member OfJson json =
        match json with
        | JObject o ->
            monad {
                let! id = o .@ "id"
                let! sub = o .@ "blah" |> map jsonObjectGetValues
                let! brand = sub .@ "brand"
                let! availability = sub .@? "availability"
                return NestedItem {
                    Item.Id = id
                    Brand = brand
                    Availability = availability
                }
            }
        | x -> Decode.Fail.objExpected x


let tag prop codec =
    Codec.ofConcrete codec
    |> Codec.compose (
                        (fun o -> match Seq.toList o with [KeyValue(p, JObject a)] when p = prop -> Ok a | _ -> Decode.Fail.propertyNotFound prop o), 
                        (fun x -> if Seq.isEmpty x then zero else Dict.toIReadOnlyDictionary (dict [prop, JObject x]))
                     )
    |> Codec.toConcrete

type Vehicle =
   | Bike
   | MotorBike of unit
   | Car       of make : string
   | Van       of make : string * capacity : float
   | Truck     of make : string * capacity : float
   | Aircraft  of make : string * capacity : float
with
    static member JsonObjCodec =
        [
            (fun () -> Bike) <!> jreq "bike"      (function  Bike             -> Some ()     | _ -> None)
            MotorBike        <!> jreq "motorBike" (function (MotorBike ()   ) -> Some ()     | _ -> None)
            Car              <!> jreq "car"       (function (Car  x         ) -> Some  x     | _ -> None)
            Van              <!> jreq "van"       (function (Van (x, y)     ) -> Some (x, y) | _ -> None)
            tag "truck" (
                (fun m c -> Truck (make = m, capacity = c))
                    <!> jreq  "make"     (function (Truck (make     = x)) -> Some  x | _ -> None)
                    <*> jreq  "capacity" (function (Truck (capacity = x)) -> Some  x | _ -> None))
            tag "aircraft" (
                (fun m c -> Aircraft (make = m, capacity = c))
                    <!> jreq  "make"     (function (Aircraft (make     = x)) -> Some  x | _ -> None)
                    <*> jreq  "capacity" (function (Aircraft (capacity = x)) -> Some  x | _ -> None))
        ] |> jchoice

type Name = {FirstName: string; LastName: string} with
    static member ToJson x = toJson (x.LastName + ", " + x.FirstName)
    static member OfJson x =
        match x with
        | JString x when String.contains ',' x -> Ok { FirstName = (split [|","|] x).[0]; LastName = (split [|","|] x).[1] }
        | JString _ -> Error "Expected a ',' separator"
        | _ -> Error "Invalid Json Type"
        
let strCleanUp x = System.Text.RegularExpressions.Regex.Replace(x, @"\s|\r\n?|\n", "")
let strCleanUpAll x = System.Text.RegularExpressions.Regex.Replace(x, "\s|\r\n?|\n|\"|\\\\", "")
type Assert with
    static member inline JSON(expected: string, value: 'a) =
        Assert.Equal("", expected, strCleanUp ((toJson value).ToString()))


open FsCheck
open FsCheck.GenOperators

type ArraySegmentGenerator =
  static member ArraySegment() =
      Arb.Default.Array()
      |> Arb.convert ArraySegment<int> (fun (s:ArraySegment<int>) -> s.ToArray())
      
let tests = [
        testList "From JSON" [
            test "item with missing key" {
                let actual : Item ParseResult = parseJson """{"id": 1, "brand": "Sony"}"""
                let expected = 
                    { Item.Id = 1
                      Brand = "Sony"
                      Availability = None }
                Assert.Equal("item", Success expected, actual)
            }

            test "nested item" {
                let actual: NestedItem ParseResult = parseJson """{"id": 1, "blah": {"brand": "Sony", "availability": "1 week"}}"""
                let expected = 
                    NestedItem {
                        Item.Id = 1
                        Brand = "Sony"
                        Availability = Some "1 week"
                    }
                Assert.Equal("item", Success expected, actual)
            }

            test "attribute ok" {
                let actual : Attribute ParseResult = parseJson """{"name": "a name", "value": "a value"}"""
                let expected = 
                    { Attribute.Name = "a name"
                      Value = "a value" }
                Assert.Equal("attribute", Success expected, actual)
            }

            test "attribute with null name" {
                let actual : Attribute ParseResult = parseJson """{"name": null, "value": "a value"}"""
                match actual with
                | Success a -> failtest "should have failed"
                | Failure e -> ()
            }

            test "attribute with null value" {
                let actual : Attribute ParseResult = parseJson """{"name": "a name", "value": null}"""
                let expected = 
                    { Attribute.Name = "a name"
                      Value = null }
                Assert.Equal("attribute", Success expected, actual)           
            }

            test "Person recursive" {
                let actual : Person ParseResult = parseJson """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""
                let expectedPerson = 
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
                Assert.Equal("Person", Success expectedPerson, actual)
            }
            #if SYSTEMJSON
            test "DateTime with milliseconds" {
                let actual : DateTime ParseResult = ofJson (JsonPrimitive "2014-09-05T04:38:07.862Z")
                let expected = new DateTime(2014,9,5,4,38,7,862)
                Assert.Equal("DateTime", Success expected, actual)
            }

            test "DateTime without milliseconds" {
                let actual : DateTime ParseResult = ofJson (JsonPrimitive "2014-09-05T04:38:07Z")
                let expected = new DateTime(2014,9,5,4,38,7)
                Assert.Equal("DateTime", Success expected, actual)
            }
            #endif
        ]

        testList "To JSON" [
            test "item with missing key" {
                let actual = 
                    { Item.Id = 1; Brand = "Sony"; Availability = None }
                    |> toJson
                    |> string
            #if NEWTONSOFT
                let expected = """{"id": 1, "brand": "Sony"}"""
            #endif
            #if FSHARPDATA
                let expected = """{"id": 1, "brand": "Sony"}"""
            #endif
            #if SYSTEMJSON
                let expected = """{"brand": "Sony", "id": 1}"""
            #endif
                    
                Assert.Equal("item", strCleanUp expected, strCleanUp actual)
            }

            test "int" {
                Assert.JSON("2", 2)
            }

            test "decimal" {
            #if FSHARPDATA
                let actual : int ParseResult = parseJson "2.1"
                Assert.Equal("decimal", Error (), Result.mapError (fun _-> ()) actual)
            #else
                let actual : int ParseResult = parseJson "2.1"
                Assert.Equal("decimal", Success 2, actual)
            #endif
            }

            test "tuple 2" {
                let expected = 
                    "[1,2]"
                Assert.JSON(expected, (1,2))
            }

            test "DateTime" {
                let expected = 
                #if NEWTONSOFT
                    "2000-03-01T16:23:34.000Z"
                #else
                    "\"2000-03-01T16:23:34.000Z\""
                #endif
                Assert.JSON(expected, DateTime(2000, 3, 1, 16, 23, 34))
            }

            test "DateTime with milliseconds" {
                let expected = 
                #if NEWTONSOFT
                    "2000-03-01T16:23:34.123Z"
                #else
                    "\"2000-03-01T16:23:34.123Z\""
                #endif
                Assert.JSON(expected, DateTime(2000, 3, 1, 16, 23, 34, 123))
            }

            test "DateTimeOffset" {
                let expected = 
                #if NEWTONSOFT
                    "2000-03-01T16:23:34.000+03:00"
                #else
                    "\"2000-03-01T16:23:34.000+03:00\""
                #endif
                Assert.JSON(expected, DateTimeOffset(2000, 3, 1, 16, 23, 34, TimeSpan(3, 0, 0)))
            }

            test "DateTimeOffset with milliseconds" {
                let expected = 
                #if NEWTONSOFT
                    "2000-03-01T16:23:34.078+03:00"
                #else
                    "\"2000-03-01T16:23:34.078+03:00\""
                #endif
                Assert.JSON(expected, DateTimeOffset(2000, 3, 1, 16, 23, 34, 78, TimeSpan(3, 0, 0)))
            }

            test "Person" {
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
                #if NEWTONSOFT
                let expected = """{"name":"John","age":44,"children":[{"name":"Katy","age":5,"children":[]},{"name":"Johnny","age":7,"children":[]}]}"""
                Assert.JSON(expected, p)
                #endif
                #if FSHARPDATA
                let expected = """{"name":"John","age":44,"children":[{"name":"Katy","age":5,"children":[]},{"name":"Johnny","age":7,"children":[]}]}"""
                Assert.JSON(expected, p)
                #endif
                #if SYSTEMJSON
                let expected = """{"age":44,"children":[{"age":5,"children":[],"name":"Katy"},{"age":7,"children":[],"name":"Johnny"}],"name":"John"}"""
                Assert.JSON(expected, p)
                #endif

            }

            test "Vehicle" {
                let u = [ Bike                       ] |> toJson |> string |> strCleanUpAll
                let v = [ MotorBike ()               ] |> toJson |> string |> strCleanUpAll
                let w = [ Car "Renault"              ] |> toJson |> string |> strCleanUpAll
                let x = [ Van ("Fiat", 5.8)          ] |> toJson |> string |> strCleanUpAll
                let y = [ Truck ("Ford", 20.0)       ] |> toJson |> string |> strCleanUpAll
                let z = [ Aircraft ("Airbus", 200.0) ] |> toJson |> string |> strCleanUpAll
            
                #if FSHARPDATA
                let expectedU = "\"[{bike:[]}]\""
                let expectedV = "\"[{motorBike:[]}]\""
                let expectedW = "\"[{car:Renault}]\""
                let expectedX = "\"[{van:[Fiat,5.8]}]\""
                let expectedY = "\"[{truck:{make:Ford,capacity:20}}]\""
                let expectedZ = "\"[{aircraft:{make:Airbus,capacity:200}}]\""
                #endif
                #if NEWTONSOFT
                let expectedU = "[{bike:[]}]"
                let expectedV = "[{motorBike:[]}]"
                let expectedW = "[{car:Renault}]"
                let expectedX = "[{van:[Fiat,5.8]}]"
                let expectedY = "[{truck:{make:Ford,capacity:20.0}}]"
                let expectedZ = "[{aircraft:{make:Airbus,capacity:200.0}}]"
                #endif
                #if SYSTEMJSON
                let expectedU = "\"[{bike:[]}]\""
                let expectedV = "\"[{motorBike:[]}]\""
                let expectedW = "\"[{car:Renault}]\""
                let expectedX = "\"[{van:[Fiat,5.8]}]\""
                let expectedY = "\"[{truck:{capacity:20,make:Ford}}]\""
                let expectedZ = "\"[{aircraft:{capacity:200,make:Airbus}}]\""
                #endif
                Assert.JSON(expectedU, u)
                Assert.JSON(expectedV, v)
                Assert.JSON(expectedW, w)
                Assert.JSON(expectedX, x)
                Assert.JSON(expectedY, y)
                Assert.JSON(expectedZ, z)
            
            }

            test "Map with null key" {
                let p: Map<string, _> = Map.ofList [null, "a"]
                Assert.JSON("{}", p)
            }

            test "JObj with null key" {
                let j = jobj [null, JString "a"]
                Assert.Equal("json", expected = "{}", actual = strCleanUp(j.ToString()))
            }
        ]

        testList "Codec" [
            test "binary" {
                let itemBinaryCodec =
                    Item.JsonObjCodec
                    |> Codec.compose jsonObjToValueCodec
                    |> Codec.compose jsonValueToTextCodec
                    //A unique overload for method 'GetString' could not be determined based on type information prior to this program point.
                    //A type annotation may be needed. Candidates:
                    //Encoding.GetString(bytes: ReadOnlySpan<byte>) : string
                    //Encoding.GetString(bytes: byte []) : string
                    |> Codec.invmap (Encoding.UTF8.GetString : byte [] -> string) Encoding.UTF8.GetBytes
                
                let actual = 
                    { Item.Id = 1; Brand = "Sony"; Availability = None }
                    |> snd itemBinaryCodec  // go to bytes
                    |> fst itemBinaryCodec  // and come back to Item

                let expected = Ok { Item.Id = 1; Brand = "Sony"; Availability = None }
                    
                Assert.Equal("item", expected, actual)
            }
        ]

        testList "Errors" [
            test "ParseError" {
                let js = """{"age" 42, "children": [], "name": "John"}"""
                let x = parseJson<Person> js
                let actual =
                    match x with
                    | Error (ParseError _) -> "ParseError"
                    | Error s -> string s
                    | Ok s -> string s 
                Assert.Equal ("Expecting a ParseError (since age is missing :)", "ParseError", actual)
            }
            test "PropertyNotFound" {
                let js = """{"ageeee": 42, "children": [], "name": "John"}"""
                let x = parseJson<Person> js
                let actual =
                    match x with
                    | Error (PropertyNotFound (s, _)) -> s
                    | s -> string s
                Assert.Equal ("Expecting a PropertyNotFound (age)", "age", actual)
            }
            test "Uncategorized" {                
                let x = ofJson<Name> (JString "aaa")
                let actual =
                    match x with
                    | Error (Uncategorized s) -> "Uncategorized:" + s
                    | s -> string s
                Assert.Equal ("Expecting an Uncategorized error (Expected a ',' separator)", "Uncategorized:Expected a ',' separator", actual)
            }
        ]



        testList "Roundtrip" [
            let inline roundtripEq (isEq: 'a -> 'a -> bool) p =
                let actual = p |> toJson |> ofJson
                let ok = 
                    match actual with
                    | Success actual -> isEq actual p
                    | _ -> false
                if not ok then printfn "Got %A from %A" actual p
                ok

            let inline roundtrip p = roundtripEq (=) p

            let testProperty name = testPropertyWithConfig { Config.Default with MaxTest = 10000; Arbitrary=[typeof<ArraySegmentGenerator>] } name

            let kvset = Seq.map (fun (KeyValue(k,v)) -> k,v) >> set

            let attributeArb = 
                lazy (gen {
                    let! name = Arb.generate |> Gen.suchThat ((<>) null)
                    let! value = Arb.generate
                    return { Attribute.Name = name; Value = value }
                } |> Arb.fromGen)

            let mapArb : Lazy<Arbitrary<Map<string, char>>> =
                lazy (gen {
                    let! keyvalues = Arb.generate
                    return keyvalues |> List.filter (fun (k,_) -> k <> null) |> Map.ofList
                } |> Arb.fromGen)

            yield testProperty "int" (roundtrip<int>)
            //yield testProperty "uint32" (roundtrip<uint32>) // not handled by FsCheck
            yield testProperty "int64" (roundtrip<int64>)
            //yield testProperty "float" (roundtrip<float>) // wrong error due to nan <> nan
            //yield testProperty "float32" (roundtrip<float32>)  // not handled by FsCheck
            yield testProperty "string" (roundtrip<string>)
            yield testProperty "decimal" (roundtrip<decimal>)
            yield testProperty "DateTime" (roundtrip<DateTime>)
            yield testProperty "DateTimeOffset" (roundtrip<DateTimeOffset>)
            yield testProperty "char" (roundtrip<char>)
            yield testProperty "byte" (roundtrip<byte>)
            yield testProperty "sbyte" (roundtrip<sbyte>)
            yield testProperty "Guid" (roundtrip<Guid>)
            yield testProperty "attribute" (Prop.forAll attributeArb.Value roundtrip<Attribute>)
            yield testProperty "string list" (roundtrip<string list>)
            yield testProperty "string set" (roundtrip<string Set>)
            yield testProperty "int array" (roundtrip<int array>)
            yield testProperty "int ArraySegment" (roundtripEq<int ArraySegment> (Seq.forall2 (=)))
            yield testProperty "int ResizeArray" (fun (x: int ResizeArray) -> roundtripEq (Seq.forall2 (=)) x)
            yield testProperty "Map<string, char>" (Prop.forAll mapArb.Value roundtrip<Map<string, char>>)
            yield testProperty "Dictionary<string, int>" (fun (x: Dictionary<string, int>) -> roundtripEq (fun a b -> kvset a = kvset b) x)
            yield testProperty "int option array" (roundtrip<int option array>)
            //yield testProperty "int Nullable array" (roundtrip<int Nullable array>) // not handled by FsCheck
            yield testProperty "decimal tuple" (roundtrip<decimal * decimal>)
            yield testProperty "Choice<(int * string) list, Choice<decimal option, string>>" (roundtrip<Choice<(int * string) list, Choice<decimal option, string>>>)

            yield test "null string" {
                let a: string = null
                if not (roundtrip a) then failtest ""
            }

            yield test "null nullable" {
                let a = Nullable<int>()
                if not (roundtrip a) then failtest ""
            }

            yield test "nullable with value" {
                let a = Nullable 2
                if not (roundtrip a) then failtest ""
            }
        ]
    ]

[<EntryPoint>]
let main _ = 
    printfn "Running tests..."
(*
    let tests = 
        tests 
        |> Test.replaceTestCode (fun name testCode ->
                                    test name {
                                        //printfn "start %s" name
                                        testCode()
                                        //printfn "finished %s" name
                                        printf "."
                                    })
*)
    runParallel (TestList (tests @ Lenses.tests))
