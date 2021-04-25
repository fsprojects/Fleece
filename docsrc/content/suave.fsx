(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/System.Json.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/Fleece.SystemJson.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/FSharpPlus.dll"
#r @"../../packages/docs/Suave/lib/netstandard2.1/Suave.dll"


(**
## Suave

In this page we will get an overview of how you can use Fleece together with Suave.

A minimal integration can be done by

*)

open Suave
open Suave.Http
open Suave.Operators
open System.IO
open System.Text
// Fleece and Json related:
open System.Json
open Fleece.SystemJson
open Fleece.SystemJson.Operators

module BusinessApp=
    [<RequireQualifiedAccess>]
    module Json =
        let inline OK (dataObj) : WebPart=
            let str = toJson dataObj |> string
            Successful.OK str
            >=> Writers.setMimeType "application/json; charset=utf-8"

        let inline parseRequestForm (ctx : HttpContext) =
            let body = ctx.request.rawForm |> Encoding.UTF8.GetString
            parseJson body

(**
In the web API part of your business app you would then do something like the code below:
*)

open BusinessApp

type Person = { Name : string }
with
    static member JsonObjCodec =
        fun name -> { Name = name }
        <!> jreq  "name" (Some << fun x -> x.Name)

let personHandler : WebPart =
    warbler (fun ctx ->
            match Json.parseRequestForm ctx with // instead of using mapJson
            | Ok (person:Person)->
                Json.OK person
            // and ideally we would deal with case when the parsing fails as well
    )
