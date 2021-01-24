(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/System.Json.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/Fleece.SystemJson.dll"
#r @"../../src/Fleece.SystemJson/bin/Release/netstandard2.1/FSharpPlus.dll"
#r @"../../packages/docs/TaskBuilder.fs/lib/net46/TaskBuilder.fs.dll"

module Giraffe=
    open System.Threading.Tasks
    open System.IO
    /// fake definition
    type Request()=
        member __.Body:Stream = failwith "not implemented"
    type HttpContext ()=
        member __.Request : Request=failwith "not implemented"
        member __.SetContentType(s:string)=failwith "not implemented"
        member __.WriteBytesAsync(b:byte array) : Task<HttpContext option> =failwith "not implemented"

    type HttpFuncResult = Task<HttpContext option>

    type HttpFunc = HttpContext -> HttpFuncResult

    type HttpHandler = HttpFunc -> HttpFunc


(**
## Giraffe

In this page we will get an overview of how you can use Fleece together with Giraffe.

A minimal integration can be done by looking at how Giraffe implements the method [WriteJsonAsync](https://github.com/giraffe-fsharp/Giraffe/blob/37e69a54d1e85649968705f13cab77abe2d0a928/src/Giraffe/ResponseWriters.fs#L53-L57) and function [json](https://github.com/giraffe-fsharp/Giraffe/blob/37e69a54d1e85649968705f13cab77abe2d0a928/src/Giraffe/ResponseWriters.fs#L186-L188):

*)


open Giraffe
open System.IO
open System.Text
// task computation builder from TaskBuilder.fs:
open FSharp.Control.Tasks.V2.ContextInsensitive
// Fleece and Json related:
open System.Json
open Fleece.SystemJson
open Fleece.SystemJson.Operators

module BusinessApp=
    module Json =
        let inline json (dataObj ) : HttpHandler =
            fun (_ : HttpFunc) (ctx : HttpContext) ->
                ctx.SetContentType "application/json; charset=utf-8"
                toJson dataObj // turn dataObj into Json
                |> string // get the Json string
                |> Encoding.UTF8.GetBytes // turn the string into bytes
                |> ctx.WriteBytesAsync // write bytes to the response

        let inline bindJsonAsync (ctx : HttpContext) =
            task {
                use reader = new StreamReader(ctx.Request.Body)
                let! body = reader.ReadToEndAsync()
                return parseJson body
            }

(**
In the web API part of your business app you would then do something like the code below:
*)

open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
// we open the Json helpers we defined last in order to avoid using the default "json" function from Giraffe:
open BusinessApp.Json
type Person = { Name : string }
with
    static member JsonObjCodec =
        fun name -> { Name = name }
        <!> jreq  "name" (Some << fun x -> x.Name)

let personHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! bindJsonAsync ctx with // instead of using ctx.BindJsonAsync we use the function above
            | Ok (person:Person)->
                return! json person next ctx
            // and ideally we would deal with case when the parsing fails as well
        }

(**
The benefit of doing an integration in this way is:

- You get a compilation error when trying to use types that don't have the proper functions defined.
- You avoid having to use runtime reflection to bind and serialize the Json.
- You have more control over the serialization than with System.Text.Json or Newtonsoft.Json
*)