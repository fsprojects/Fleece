(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/net461/Newtonsoft.Json.dll"
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/net461/Fleece.NewtonsoftJson.dll"
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/net461/FSharpPlus.dll"

open System
open Newtonsoft.Json
open FSharpPlus
open FSharpPlus.Builders
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators

(**
## Comparison with Json.Net or Newtonsoft Json

In order to be compatible with Newtonsoft Json conventions you need to either specify a constructor or have a default constructor with the same name as the public field
(note the different first letter casing).
*)

type User(userName:string , enabled: bool)=
    member __.UserName = userName
    member __.Enabled = enabled
let userJson="""
{"userName":"test","enabled":true}
"""
let user = JsonConvert.DeserializeObject<User> userJson
(**
Another alternative would be to use CLI-mutable
*)
[<CLIMutable>]
type UserR ={ UserName:string; Enabled:bool }
(**
This enables Json.Net to deserialize json into your structure but leave the F# code easier to reason about.
*)
let userRecord = JsonConvert.DeserializeObject<UserR> userJson
(**
### Controlling polymorphism

The default approach is to use [serialization binder](https://www.newtonsoft.com/json/help/html/SerializeSerializationBinder.htm). The assumption is that you have an abstract class or an interface that implemented by many different types.

In order to have a better serialization of union cases you need to implement something as seen in [FsCodec.NewtonsoftJson/UnionConverter](https://github.com/jet/FsCodec/blob/2bdcd60c04588c81caecbea6e5507348c4763fd9/src/FsCodec.NewtonsoftJson/UnionConverter.fs).

Since UnionConverter does not map well to F# concepts you might end up with a similar pattern as seen in Fleece. For instance if you read [Eirik Tsarpalis blog](https://eiriktsarpalis.wordpress.com/2018/10/30/a-contract-pattern-for-schemaless-datastores/).

Fleece lets you decode the Json at both a lower and hight level. This allows you also to mix and match with the native Json library (in this case Newtonsoft.Json):
*)
[<CLIMutable>]
type CarInfo = { Make:string; Model:string; Trim:string}
type Vehicle =
   | Bike
   | Car       of CarInfo
with
    static member OfJson (json:Linq.JToken) =
        match json with
        | JObject o ->
            monad {
                match! o .@ "type" with
                | "Bike" -> return Bike
                | "Car" ->
                    // we know that json token is a JObject due to the check above so we can directly cast it:
                    let jobj : Linq.JObject = downcast json
                    try
                        // now we can use the default Newtonsoft Json decoder:
                        let info = jobj.ToObject<CarInfo>() // NOTE: Use this pattern with care since you hand over control to Newtonsoft.Json
                        return Car info
                    with
                    | e-> return! Decode.Fail.parseError e "Could not parse CarInfo"
                | x -> return! Uncategorized (sprintf "Unexpected type name %s" x) |> Error
            }
        | x -> Decode.Fail.objExpected x
(**
This pattern is *ugly* but can be useful. Modifying the type CarInfo above will give you runtime exceptions without a clear indication that it's a broken contract.
*)