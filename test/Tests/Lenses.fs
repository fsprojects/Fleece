module Tests.Lenses
open System
open System.Collections.Generic
open System.Linq
open Fuchu
open Fleece
open FSharpPlus
open FSharpPlus.Lens
open Fleece.Lens

#if FSHARPDATA
open FSharp.Data
open Fleece.FSharpData
type FdEncoding = Fleece.FSharpData.Encoding
let jsonValueParse = FdEncoding.Parse
#endif


#if NEWTONSOFT
open Fleece.Newtonsoft
open Newtonsoft.Json
open Newtonsoft.Json.Linq
type NsjEncoding = Fleece.Newtonsoft.Encoding
let jsonValueParse = NsjEncoding.Parse
#endif

#if SYSTEMJSON
open System.Json
open Fleece.SystemJson
type SjEncoding = Fleece.SystemJson.Encoding
let jsonValueParse = SjEncoding.Parse
#endif

#if SYSTEMTEXTJSON
open Fleece.SystemTextJson
open System.Text.Json
type StjEncoding = Fleece.SystemTextJson.Encoding
let jsonValueParse = StjEncoding.Parse
#endif

let strCleanUp x = System.Text.RegularExpressions.Regex.Replace(x, @"\s|\r\n?|\n", "")

let tests = [
        testList "key" [
            test "example 1: read first key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "a" << _JBool)
                let expected = true
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2: read second key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "b" << _JNumber)
                let expected = 200m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3.1: read with missing key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" ) ^? (_jkey "c" << _JNumber)
                Assert.Equal("item", None, actual)
            }
            test "example 3.2: read with missing key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" ) ^? _jkey "c"
                Assert.Equal("item", None, actual)
            }
            test "example 4.1: write with missing key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" )|> (_jkey "c" ) .-> JString "a"
                let expected = jsonValueParse ("{\"a\": true, \"b\": 200, \"c\":\"a\"}")
                Assert.Equal("item", strCleanUp (string expected), strCleanUp (string actual))
            }
            test "example 4.2: write with missing key" { //TODO: Fix
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" )|> (_jkey "c" << _JString) .-> "a"
                let expected = jsonValueParse ("{\"a\": true, \"b\": 200, \"c\":\"a\"}")
                //Assert.Equal("item", string expected, string actual)
                printfn "todo: %A ~ %A" expected actual
            }
            test "example 5: write existing key" {
                let actual = jsonValueParse( "{\"a\": true, \"b\": 200}" )|> (_jkey "a" << _JBool) .-> false
                let expected = jsonValueParse ("{\"a\": false, \"b\": 200}")
                Assert.Equal("item", strCleanUp (string expected), strCleanUp (string actual))
            }
            test "example 6: read key from a different type" {
                let actual = jsonValueParse( "[1,2,3]" ) ^? _jkey "a"
                Assert.Equal("item", true, actual.IsNone)
            }

        ]
        testList "_String" [
            test "example 1" {
                let actual = jsonValueParse ("{\"a\": \"xyz\", \"b\": true}") ^? (_jkey "a" << _JString)
                let expected = "xyz"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = jsonValueParse ("{\"a\": \"xyz\", \"b\": true}") ^? (_jkey "b" << _JString)
                Assert.Equal("item", None, actual)
            }
            (* test "example 3" {
                let actual = JString "a" |>  _JString .-> "b"
                let expected = JString "b" 
                Assert.Equal("item", string expected, string actual)
            } *)
        ]
        testList "_Number" [
            test "example 1" {
                let actual = jsonValueParse ("{\"a\": 100, \"b\": true}") ^? (_jkey "a" << _JNumber)
                let expected = 100m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2: write" {
                let actual = jsonValueParse ("{\"a\": 100, \"b\": true}") |> (_jkey "a" << _JNumber) .-> 200m
                let expected =
                                #if NEWTONSOFT
                                "{\"a\": 200.0, \"b\": true}"
                                #else
                                "{\"a\": 200, \"b\": true}"
                                #endif
                Assert.Equal("item", strCleanUp (string (jsonValueParse expected)), strCleanUp (string actual))
            }
        ]
        testList "array" [
            test "example 1" {
                let actual = jsonValueParse ("[\"a\"]") ^? (_jnth 0 << _JString)
                let expected = "a"
                Assert.Equal("item", Some expected, actual)
            }
            test "example 2" {
                let actual = jsonValueParse ("[123]") ^? (_jnth 0 << _JNumber)
                let expected = 123m
                Assert.Equal("item", Some expected, actual)
            }
            test "example 3.1: read for missing index" {
                let actual = jsonValueParse ("[1,2,3]") ^? (_jnth 4 << _JNumber)
                Assert.Equal("item", None, actual)
            }
            test "example 3.2: read for missing index" {
                let actual = jsonValueParse ("[1,2,3]") ^? _jnth 4
                Assert.Equal("item", None, actual)
            }
            test "example 4: write" {
                let actual = jsonValueParse ("[1,2,3]") |> (_jnth 1  << _JNumber) .-> 2.5m
                let expected = jsonValueParse ("[1,2.5,3]")
                Assert.Equal("item", string expected, string actual)
            }
            test "example 5: write for missing index" {
                let actual = jsonValueParse ("[1]") |> (_jnth 1 << _JString) .-> "a"
                let expected = jsonValueParse ("[1]")
                Assert.Equal("item", string expected, string actual)
            }
        ]
    ]
