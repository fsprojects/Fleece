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
            test "example 1" {
                let actual = JsonValue.Parse( "{\"a\": 100, \"b\": 200}" ) ^? (key "a" << _Number)
                let expected = 100m 
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse( "[1,2,3]" ) ^? key "a"
                Assert.Equal("item", None, actual)
            }
        ]
        testList "_String" [
            test "example 1" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (key "a" << _String)
                let expected = "xyz"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = JsonValue.Parse ("{\"a\": \"xyz\", \"b\": true}") ^? (key "b" << _String)
                Assert.Equal("item", None, actual)
            }
        ]
    ]
