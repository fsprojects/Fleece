module Tests.Tests
open Fuchu
open Fleece.Redis
open Fleece.Redis.Operators
open FSharpPlus
open StackExchange.Redis
type Person = {
    Name: string
    Age: int
}

type Person with
    static member Create name age = { Person.Name = name; Age = age }

    static member OfRedis o = 
        Person.Create <!> (o .@ "name") <*> (o .@ "age")
        

    static member ToRedis (x: Person) =
        [ 
            "name" .= x.Name
            "age" .= x.Age
        ]

type Item = {
    Id: int
    Brand: string
    Availability: string option
}

type Item with
    static member RedisObjCodec =
        fun id brand availability -> { Item.Id = id; Brand = brand; Availability = availability }
        <!> rreq  "id"          (fun x -> Some x.Id     )
        <*> rreq  "brand"       (fun x -> Some x.Brand  )
        <*> ropt "availability" (fun x -> x.Availability)
        |> Codec.ofConcrete

open FsCheck
open FsCheck.GenOperators
let personHashEntries = [ HashEntry(implicit "name", implicit "John"); HashEntry(implicit "age", implicit 44) ]
let person = { Person.Name = "John"; Age = 44 }
let itemHashEntries = [HashEntry(implicit "id", implicit 11); HashEntry(implicit "brand", implicit "Spinal trap")]
let item = {Id=11; Brand="Spinal trap"; Availability= None}
let tests = [
        testList "From Redis" [

            test "Person" {
                let actual : Person ParseResult =Person.OfRedis personHashEntries
                Assert.Equal("Person", Ok person, actual)
            }
            test "Item" {
                let actual : Item ParseResult = Codec.decode Item.RedisObjCodec itemHashEntries
                Assert.Equal("Item", Ok item, actual)
            }
        ]
        testList "To Redis" [
            test "Person" {
                let actual = Person.ToRedis person
                Assert.Equal("Person", personHashEntries, actual)
            }
            test "Item" {
                let actual = Codec.encode Item.RedisObjCodec item
                Assert.Equal("Item", itemHashEntries, actual)
            }
        ]
    ]

[<EntryPoint>]
let main _ = 
    printfn "Running tests..."
    runParallel (TestList (tests))
