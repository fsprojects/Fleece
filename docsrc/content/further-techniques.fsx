(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r "nuget: System.Json"
#r "nuget: Fleece.SystemJson"
#r "nuget: FSharpPlus"

open Fleece.SystemJson
open Fleece.SystemJson.Operators

(**
Sometimes, the JSON required by a given situation will contain fields that do not need to be present in the F# data model.
For example, the JSON-RPC 2.0 specification requires every request/response object to carry the field `jsonrpc` with value `"2.0"`.
In a codebase that only uses JSON-RPC 2.0, why capture this field on a record?

When writing `ToJson` and `OfJson` methods for this data, handling the required field is fairly natural:
*)

type Request =
    { Method: string
      MethodParams: Map<string, string>}
    static member ToJson (r: Request) =
        jobj [
            "method"  .= r.Method
            "params"  .= r.MethodParams
            "jsonrpc" .= "2.0"
        ]
    static member OfJson json =
        match json with
        | JObject o ->
            let method = o .@ "method"
            let methodParams = o .@ "params"
            // We require the "jsonrpc" field to be present
            let jsonrpc = o .@ "jsonrpc"
            match method, methodParams, jsonrpc with
            | Decode.Success m, Decode.Success p, Decode.Success "2.0" -> // We enforce the value of the field
                // ...but do not use it in the final object
                Decode.Success {
                    Method = m
                    MethodParams = p
                }
            | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
        | x -> Decode.Fail.objExpected x

(**
The can also be modeled with Codecs:
*)

type Response =
    { Result: string option
      Error: string option }
      static member JsonObjCodec =
          fun r e _ -> { Result = r; Error = e }
          |> withFields
          |> jfieldOpt "result"  (fun r -> r.Result)
          |> jfieldOpt "error"   (fun r -> r.Error)
          |> jfield    "jsonrpc" (fun _ -> "2.0")

(**
There are three parts to this.
First, the constructor is given an unused third parameter, which will receive the field required on the JSON object.
Second, the `"jsonrpc"` field is required using `jfield`; its getter always returns `"2.0"`
Finally: the fields must be in the correct order -- that is, the field specs must follow the order of the arguments in the constructor.
*)
