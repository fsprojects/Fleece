(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/netstandard2.1/Newtonsoft.Json.dll"
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/netstandard2.1/Fleece.NewtonsoftJson.dll"
#r @"../../src/Fleece.NewtonsoftJson/bin/Release/netstandard2.1/FSharpPlus.dll"

open System
open Newtonsoft.Json
open FSharpPlus
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
                        let info = jobj.ToObject<CarInfo>() // NOTE: here we hand over control of the mapping to Newtonsoft.Json
                        return Car info
                    with
                    | e-> return! Decode.Fail.parseError e "Could not parse CarInfo"
                | x -> return! Uncategorized (sprintf "Unexpected type name %s" x) |> Error
            }
        | x -> Decode.Fail.objExpected x
(**
This pattern is *ugly* but can be useful. Modifying the type CarInfo above will give you runtime exceptions without a clear indication that it's a broken contract.

One of the useful things about having a mixed approach as seen above is that you can gradually convert to say Fleece in a large codebase without having to fix everything at once.
*)

(**
## Full control over mapping

The default approach to serialization and deserialization in Fleece let you have a lot of control. You choose exactly how it should work.

It's easy to let the structure of your Json be completely independent of the structure of your data. Newtonsoft assumes that what you want follow a lot of conventions.

If we look at a simple example of the Json not matching the representation (where you would need a custom JsonConverter):
*)

type Person = {
    Name : string * string
}
with
    static member ToJson (x: Person) =
        jobj [
            "firstname" .= fst x.Name
            "lastname" .= snd x.Name
        ]
    static member OfJson json =
        match json with
        | JObject o ->
            let firstname = jget o "firstname"
            let lastname = jget o "lastname"
            match firstname, lastname with
            | Decode.Success firstname, Decode.Success lastname ->
                Decode.Success {
                    Person.Name = (firstname,lastname)
                }
            | x -> Error <| Uncategorized (sprintf "Error parsing person: %A" x)
        | x -> Decode.Fail.objExpected x

(**
In that sense, having access to functions helps us make what in Newtonsoft is a pain to implement, very easy.
*)
