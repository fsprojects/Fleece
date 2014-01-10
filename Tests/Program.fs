open Fuchu
open Fleece
open System.Json
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
        | JObject o -> Person.Create <!> (o .> "name") <*> (o .> "age") <*> (o .> "children")
        | x -> Failure ("Expected person, found " + x.ToString())

type Attribute = {
    Name: string
    Value: string
}

type Attribute with
    static member instance (FromJSON, _: Attribute, _: Attribute ParseResult) =
        function
        | JObject o -> 
            monad {
                let! name = o .> "name"
                if name = null then 
                    return! Failure "Attribute name was null"
                else
                    let! value = o .> "value"
                    return {
                        Attribute.Name = name
                        Value = value
                    }
            }
        | x -> Failure ("Expected Attribute, found " + x.ToString())

let tests = 
    testList "From JSON" [
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

[<EntryPoint>]
let main argv = 
    run tests
