open System
open ReadOnlyCollectionsExtensions
open System.Collections.Generic
open System.Json
open System.Linq
open Fuchu
open Fleece
open Fleece.Operators
open FSharpPlus

type Person = {
    Name: string
    Age: int
    Children: Person list
}

type Person with
    static member Create name age children = { Person.Name = name; Age = age; Children = children }

    static member FromJSON (_: Person) = 
        function
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member ToJSON (x: Person) =
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
                    return! Failure "Attribute name was null"
                else
                    let! value = o .@ "value"
                    return {
                        Attribute.Name = name
                        Value = value
                    }
            }
        | x -> Failure (sprintf "Expected Attribute, found %A" x)

    static member ToJSON (x: Attribute) =
        jobj [ "name" .= x.Name; "value" .= x.Value ]

type Item = {
    Id: int
    Brand: string
    Availability: string option
}

type Item with
    static member FromJSON (_: Item) =
        function
        | JObject o ->
            monad {
                let! id = o .@ "id"
                let! brand = o .@ "brand"
                let! availability = o .@? "availability"
                return {
                    Item.Id = id
                    Brand = brand
                    Availability = availability
                }
            }
        | x -> Failure (sprintf "Expected Item, found %A" x)

type NestedItem = NestedItem of Item

type NestedItem with
    static member FromJSON (_: NestedItem) =
        function
        | JObject o ->
            monad {
                let! id = o .@ "id"
                let! sub = o .@ "blah" |> map JsonObject.GetValues
                let! brand = sub .@ "brand"
                let! availability = sub .@? "availability"
                return NestedItem {
                    Item.Id = id
                    Brand = brand
                    Availability = availability
                }
            }
        | x -> Failure (sprintf "Expected Item, found %A" x)
        

type Assert with
    static member inline JSON(expected: string, value: 'a) =
        Assert.Equal("", expected, (toJSON value).ToString())


open FsCheck
open FsCheck.GenOperators

let tests = 
    TestList [
        testList "From JSON" [
            test "item with missing key" {
                let actual : Item ParseResult = parseJSON """{"id": 1, "brand": "Sony"}"""
                let expected = 
                    { Item.Id = 1
                      Brand = "Sony"
                      Availability = None }
                Assert.Equal("item", Success expected, actual)
            }

            test "nested item" {
                let actual: NestedItem ParseResult = parseJSON """{"id": 1, "blah": {"brand": "Sony", "availability": "1 week"}}"""
                let expected = 
                    NestedItem {
                        Item.Id = 1
                        Brand = "Sony"
                        Availability = Some "1 week"
                    }
                Assert.Equal("item", Success expected, actual)
            }

            test "attribute ok" {
                let actual : Attribute ParseResult = parseJSON """{"name": "a name", "value": "a value"}"""
                let expected = 
                    { Attribute.Name = "a name"
                      Value = "a value" }
                Assert.Equal("attribute", Success expected, actual)
            }

            test "attribute with null name" {
                let actual : Attribute ParseResult = parseJSON """{"name": null, "value": "a value"}"""
                match actual with
                | Success a -> failtest "should have failed"
                | Failure e -> ()
            }

            test "attribute with null value" {
                let actual : Attribute ParseResult = parseJSON """{"name": "a name", "value": null}"""
                let expected = 
                    { Attribute.Name = "a name"
                      Value = null }
                Assert.Equal("attribute", Success expected, actual)           
            }

            test "Person recursive" {
                let actual : Person ParseResult = parseJSON """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""
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
        ]

        testList "To JSON" [
            test "int" {
                Assert.JSON("2", 2)
            }

            test "tuple 2" {
                Assert.JSON("[1,2]", (1,2))
            }

            test "DateTime" {
                Assert.JSON("\"2000-03-01T16:23:34Z\"", DateTime(2000, 3, 1, 16, 23, 34))
            }

            test "DateTimeOffset" {
                Assert.JSON("\"2000-03-01T16:23:34+03:00\"", DateTimeOffset(2000, 3, 1, 16, 23, 34, TimeSpan(3, 0, 0)))
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
                Assert.JSON("""{"name":"John","age":44,"children":[{"name":"Katy","age":5,"children":[]},{"name":"Johnny","age":7,"children":[]}]}""", p)
            }
        ]

        testList "Roundtrip" [
            let inline roundtripEq (isEq: 'a -> 'a -> bool) p =
                let actual = p |> toJSON |> fromJSON
                let ok = 
                    match actual with
                    | Success actual -> isEq actual p
                    | _ -> false
                if not ok then printfn "Got %A from %A" actual p
                ok

            let inline roundtrip p = roundtripEq (=) p

            let testProperty name = testPropertyWithConfig { Config.Default with MaxTest = 10000 } name

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
    runParallel tests
