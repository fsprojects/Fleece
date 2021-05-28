module Tests.Lenses
open System
open System.Collections.Generic
open System.Linq
open Fuchu
open Fleece
open FSharpPlus
open FSharpPlus.Lens

#if FSHARPDATA
open FSharp.Data
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open Fleece.FSharpData.Lens
#endif
#if SYSTEMJSON
open System.Json
open Fleece.SystemJson
open Fleece.SystemJson.Operators
open Fleece.SystemJson.Lens
#endif
#if SYSTEMTEXTJSON
open Fleece.SystemTextJson.Helpers
open Fleece.SystemTextJson
open Fleece.SystemTextJson.Operators
open System.Text.Json
open Fleece.SystemTextJson.Lens
#endif
#if NEWTONSOFT
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators
open Fleece.Newtonsoft.Lens
open Newtonsoft.Json
open Newtonsoft.Json.Linq
#endif
#if FABLE
open Fleece.FableSimpleJson
open Fleece.FableSimpleJson.Operators
open Fleece.FableSimpleJson.Lens
open Fable.SimpleJson
#endif
let tests = [
        testList "key" [
            test "example 1: read first key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "a" << _JBool)
                let expected = true
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2: read second key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "b" << _JNumber)
                let expected = 200m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3: read with missing key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "c" << _JNumber)
                Assert.Equal("item", None, actual)
            }
            test "example 4.1: write with missing key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" )|> (_jkey "c" ) .-> JString "a"
                let expected = JsonValue.Parse ("{\"a\": true, \"b\": 200, \"c\":\"a\"}")
                Assert.Equal("item", string expected, string actual)
            }
            test "example 4.2: write with missing key" { //TODO: Fix
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" )|> (_jkey "c" << _JString) .-> "a"
                let expected = JsonValue.Parse ("{\"a\": true, \"b\": 200, \"c\":\"a\"}")
                //Assert.Equal("item", string expected, string actual)
                printfn "todo: %A ~ %A" expected actual
            }
            test "example 5: write existing key" {
                let actual = JsonValue.Parse( "{\"a\": true, \"b\": 200}" )|> (_jkey "a" << _JBool) .-> false
                let expected = JsonValue.Parse ("{\"a\": false, \"b\": 200}")
                Assert.Equal("item", string expected, string actual)
            }
            test "example 6: read key from a different type" {
                let actual = JsonValue.Parse( "[1,2,3]" ) ^? _jkey "a"
                Assert.Equal("item", true, actual.IsNone)
            }

        ]
        testList "_String" [
            test "example 1" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (_jkey "a" << _JString)
                let expected = "xyz"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (_jkey "b" << _JString)
                Assert.Equal("item", None, actual)
            }
            test "example 3" {
                let actual = JString "a" |>  _JString .-> "b"
                let expected = JString "b" 
                Assert.Equal("item", string expected, string actual)
            }
        ]
        testList "_Number" [
            test "example 1" {
                let actual = JsonValue.Parse ("{\"a\": 100, \"b\": true}") ^? (_jkey "a" << _JNumber)
                let expected = 100m
                Assert.Equal("item", Some expected, actual)
            }
            #if !FABLE
            test "example 2: write" {
                let actual = JsonValue.Parse ("{\"a\": 100, \"b\": true}") |> (_jkey "a" << _JNumber) .-> 200m
                let expected =
                                #if NEWTONSOFT
                                "{\"a\": 200.0, \"b\": true}"
                                #else
                                "{\"a\": 200, \"b\": true}"
                                #endif
                Assert.Equal("item", (string (JsonValue.Parse expected)), string actual)
            }
            #endif
        ]
        testList "array" [
            test "example 1" {
                let actual = JsonValue.Parse ("[\"a\"]") ^? (_jnth 0 << _JString)
                let expected = "a"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse ("[123]") ^? (_jnth 0 << _JNumber)
                let expected = 123m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3: read for missing index" {
                let actual = JsonValue.Parse ("[1,2,3]") ^? (_jnth 4 << _JNumber)
                Assert.Equal("item", None, actual)
            }
            #if FABLE
            test "example 4: write" {
                let actual = JsonValue.Parse ("[1,2,3]") |> (_jnth 1  << _JNumber) .-> 2.5
                let expected = JsonValue.Parse ("[1,2.5,3]")
                Assert.Equal("item", string expected, string actual)
            }
            #else
            test "example 4: write" {
                let actual = JsonValue.Parse ("[1,2,3]") |> (_jnth 1  << _JNumber) .-> 2.5m
                let expected = JsonValue.Parse ("[1,2.5,3]")
                Assert.Equal("item", string expected, string actual)
            }
            #endif
            test "example 5: write for missing index" {
                let actual = JsonValue.Parse ("[1]") |> (_jnth 1 << _JString) .-> "a"
                let expected = JsonValue.Parse ("[1]")
                Assert.Equal("item", string expected, string actual)
            }
        ]
    ]
