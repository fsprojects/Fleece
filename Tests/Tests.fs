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

    static member instance (FromJSON, _: Person, _: Person ParseResult) = 
        function
        | JObject o -> Person.Create <!> (o .@ "name") <*> (o .@ "age") <*> (o .@ "children")
        | x -> Failure (sprintf "Expected person, found %A" x)

    static member instance (ToJSON, x: Person, _:JsonValue) = fun () ->
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
    static member instance (FromJSON, _: Attribute, _: Attribute ParseResult) =
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

    static member instance (ToJSON, x: Attribute, _:JsonValue) = fun () ->
        jobj [ "name" .= x.Name; "value" .= x.Value ]

type Item = {
    Id: int
    Brand: string
    Availability: string option
}

type Item with
    static member instance (FromJSON, _: Item, _: Item ParseResult) =
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

type Assert with
    static member inline JSON(expected: string, value: 'a) =
        Assert.Equal("", expected, (toJSON value).ToString())

let tests = 
    TestList [
        testList "From JSON" [
            test "item with missing key" {
                let actual : Item ParseResult = parseJSON """{"id": 1, "brand": "Sony"}"""
                let expected = 
                    { Item.Id = 1
                      Brand = "Sony"
                      Availability = None }
                Assert.Equal("item", Choice1Of2 expected, actual)
            }

            test "attribute ok" {
                let actual : Attribute ParseResult = parseJSON """{"name": "a name", "value": "a value"}"""
                let expected = 
                    { Attribute.Name = "a name"
                      Value = "a value" }
                Assert.Equal("attribute", Choice1Of2 expected, actual)
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
                Assert.Equal("attribute", Choice1Of2 expected, actual)           
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
                Assert.Equal("Person", Choice1Of2 expectedPerson, actual)
            }
        ]

        testList "To JSON" [
            test "int" {
                Assert.JSON("2", 2)
            }

            test "tuple 2" {
                Assert.JSON("[1,2]", (1,2))
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
            let inline roundtrip p = 
                let actual = p |> toJSON |> fromJSON
                let ok = actual = Success p
                if not ok then printfn "Got %A from %A" actual p
                ok

            let testProperty name = testPropertyWithConfig { FsCheck.Config.Default with MaxTest = 10000 } name
            yield testProperty "int" (roundtrip<int>)
            //yield testProperty "uint32" (roundtrip<uint32>) // not handled by FsCheck
            yield testProperty "int64" (roundtrip<int64>)
            //yield testProperty "float" (roundtrip<float>) // wrong error due to nan <> nan
            //yield testProperty "float32" (roundtrip<float32>)  // not handled by FsCheck
            yield testProperty "string" (roundtrip<string>)
            yield testProperty "decimal" (roundtrip<decimal>)
            yield testProperty "char" (roundtrip<char>)
            yield testProperty "byte" (roundtrip<byte>)
            yield testProperty "sbyte" (roundtrip<sbyte>)
            yield testProperty "attribute" (roundtrip<Attribute>)
            yield testProperty "string list" (roundtrip<string list>)
            yield testProperty "string set" (roundtrip<string Set>)
            yield testProperty "int array" (roundtrip<int array>)
            yield testProperty "int option array" (roundtrip<int option array>)
            yield testProperty "decimal tuple" (roundtrip<decimal * decimal>)
            yield testProperty "Choice<(int * string) list, Choice<decimal option, string>>" (roundtrip<Choice<(int * string) list, Choice<decimal option, string>>>)
        ]
    ]

[<EntryPoint>]
let main argv = 
    runParallel tests
