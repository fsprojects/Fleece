module Tests.Lenses
open System
open System.Collections.Generic
open System.Linq
open Fuchu
open Fleece
open Fleece.Operators
open Fleece.Lens
open FSharpPlus
open FSharpPlus.Lens

#if FSHARPDATA
open FSharp.Data
#endif
#if SYSTEMJSON
open System.Json
#endif
#if NEWTONSOFT
open Newtonsoft.Json
open Newtonsoft.Json.Linq
#endif

let tests = 
    TestList [
        testList "key" [
            test "example 1: read first key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_key "a" << _Bool)
                let expected = true
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2: read second key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_key "b" << _Number)
                let expected = 200m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3: read with missing key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_key "c" << _Number)
                Assert.Equal("item", None, actual)
            }
            test "example 3" {
                let actual = JsonValue.Parse( "[1,2,3]" ) ^? _key "a"
                Assert.Equal("item", None, actual)
            }
        ]
        testList "_String" [
            test "example 1" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (_key "a" << _String)
                let expected = "xyz"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (_key "b" << _String)
                Assert.Equal("item", None, actual)
            }
        ]
        testList "Integer" [
            test "example 1" {
                let actual = JsonValue.Parse ("{\"a\": 100, \"b\": true}") ^? (_key "a" << _Number)
                let expected = 100m
                Assert.Equal("item", Some expected, actual)
            }
        ]
        testList "array" [
            test "example 1" {
                let actual = JsonValue.Parse ("[\"a\"]") ^? (_nth 0 << _String)
                let expected = "a"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse ("[123]") ^? (_nth 0 << _Number)
                let expected = 123m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3: read for missing index" {
                let actual = JsonValue.Parse ("[1,2,3]") ^? (_nth 4 << _Number)
                Assert.Equal("item", None, actual)
            }
            test "example 4: write" {
                let actual = JsonValue.Parse ("[1,2,3]") |> (_nth 1) .-> JString "a"
                let expected = JsonValue.Parse ("[1,\"a\",3]")
                Assert.Equal("item", expected, actual)
            }
            test "example 5: write for missing index" {
                let actual = JsonValue.Parse ("[1]") |> (_nth 1) .-> JString "a"
                let expected = JsonValue.Parse ("[1]")
                Assert.Equal("item", expected, actual)
            }

        ]

    ]
