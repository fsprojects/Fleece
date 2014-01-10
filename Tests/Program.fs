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
    static member instance (FromJSON, _: Person, _: Person ChoiceS) = 
        function
        | JObject o -> Person.Create <!> (o .> "name") <*> (o .> "age") <*> (o .> "children")
        | x -> Failure ("Expected person, found " + x.ToString())


let tests = 
    testList "From JSON" [
        test "Person recursive" {
            let actual : Person ChoiceS = parseJSON """{"name": "John", "age": 44, "children": [{"name": "Katy", "age": 5, "children": []}, {"name": "Johnny", "age": 7, "children": []}]}"""
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
