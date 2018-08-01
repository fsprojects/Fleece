namespace FleeceB.Benchmarks.Samples

#nowarn "40"

open Fleece
open Fleece.Operators

type ComplexType =
    { a: int list option
      b: ChildType
      c: MixinType
      r: RecursiveType option
      p: Map<int, Json> }
and ChildType =
    { d: WrapperType
      e: byte array }
and RecursiveType =
    { n: string
      child: RecursiveType list }
and MixinType =
    { f: int list }
and [<Struct>] WrapperType =
    | Sad of string

module rec Json =
    module E = Json.Encode
    module D = Json.Decode
    // module M = Mixin

    module Decode =
        let propertyBag = D.mapWithCustomKey (Fleece.Decoder.fromThrowingConverter System.Int32.Parse) D.json
        let recursiveType =
            let mk = (do ()); fun n c -> { n = n; child = c }
            let rec inner () =
                let toRecursiveType =
                    mk
                    <!> D.required D.string "n"
                    <*> (D.optional (D.listWith (D.delay inner)) "c" >=> D.withDefault [])
                D.jsonObject >=> toRecursiveType
            inner ()
        let wrapperType = D.string >-> Sad
        let mixinType =
            let toMixinType =
                (fun f -> { f = f })
                <!> D.required D.intList "f"
            D.jsonObject >=> toMixinType
        let childType =
            let toChildType =
                (fun d e -> { d = d; e = e })
                <!> D.required D.wrapperType "d"
                <*> D.required D.bytes "e"
            D.jsonObject >=> toChildType
        let complexType =
            let propertyBag = D.mapWithCustomKey (Fleece.Decoder.fromThrowingConverter System.Int32.Parse) D.json
            let toComplexType =
                (fun a b c r p -> { a = a; b = b; c = c; r = r; p = p })
                <!> D.optional D.intList "a"
                <*> D.required D.childType "b"
                <*> D.requiredMixin D.mixinType
                <*> D.optional D.recursiveType "r"
                <*> (D.optional D.propertyBag "p" >=> D.withDefault Map.empty >=> D.assertThat ((=) Map.empty) "Only empty map allowed")
            D.jsonObject >=> toComplexType
    module Mixin =
        let recursiveType =
            let rec inner x jObj =
                jObj
                |> E.required E.string "n" x.n
                |> E.ifNotEqual [] (E.listWith (E.buildWith inner)) "child" x.child
            inner
        let mixinType x jObj =
                jObj
                |> E.required E.intList "f" x.f
        let childType x jObj =
                jObj
                |> E.required Encode.wrapperType "d" x.d
                |> E.required E.bytes "e" x.e
        let complexType =
            let propertyBag = E.mapWithCustomKey string E.json
            fun x jObj ->
                jObj
                |> E.optional E.intList "a" x.a
                |> E.required E.childType "b" x.b
                |> E.requiredMixin mixinType x.c
                |> E.optional E.recursiveType "r" x.r
                |> E.ifNotEqual Map.empty propertyBag "p" x.p
    module Encode =
        let intList = E.listWith E.int
        let recursiveType = E.buildWith Mixin.recursiveType
        let wrapperType = (do ()); fun (Sad x) -> E.string x
        let mixinType = E.buildWith Mixin.mixinType
        let childType = E.buildWith Mixin.childType
        let complexType = E.buildWith Mixin.complexType

module E = Json.Encode
module D = Json.Decode
module M = Json.Mixin

type RecursiveType with
    static member ToJson x = E.recursiveType x
    static member MixinJson (x, jObj) = M.recursiveType x jObj
    static member FromJson (_:RecursiveType) = D.recursiveType
type WrapperType with
    static member ToJson x = E.wrapperType x
    static member FromJson (_:WrapperType) = D.wrapperType
type MixinType with
    static member ToJson x = E.mixinType x
    static member MixinJson (x, jObj) = M.mixinType x jObj
    static member FromJson (_:MixinType) = D.mixinType
type ChildType with
    static member ToJson x = E.childType x
    static member MixinJson (x, jObj) = M.childType x jObj
    static member FromJson (_:ChildType) = D.childType
type ComplexType with
    static member ToJson x = E.complexType x
    static member MixinJson (x, jObj) = M.complexType x jObj
    static member FromJson (_:ComplexType) = D.complexType

module Constants =
    open System.Text

    let string1 = """{"f":[1,2,3,4],"a":[2,4,6,8],"b":{"e":"SGVsbG8gd29ybGQh","d":"winter"}}"""
    let string2 = """{"a":[2,4,6,8],"b":{"d":"winter","e":"SGVsbG8gd29ybGQh"},"f":[1,2,3,4]}"""
    let string3 = """{"b":{"d":"facebook","e":""},"f":[],"r":{"n":"1","child":[{"n":"s","child":[{"n":"x"}]},{"n":"z"}]},"p":{"1":"b","2":"n","3":1,"4":[12],"5":null,"6":{"low":true}}}"""
    let strings =
        [| string1; string2; string3 |]
    let jsons =
        Array.map (Json.parse >> JsonResult.getOrThrow) strings
    let objectA =
        { a = Some [ 2; 4; 6; 8 ]
          b = { d = Sad "winter"
                e = "Hello world!" |> Encoding.UTF8.GetBytes }
          c = { f = [ 1; 2; 3; 4 ] }
          r = None
          p = Map.empty }
    let objectB =
        { a = None
          b = { d = Sad "facebook"
                e = "" |> Encoding.UTF8.GetBytes }
          c = { f = [] }
          r = Some { n = "1"
                     child = [ { n = "s"
                                 child = [ { n = "x"
                                             child = [] } ] }
                               { n = "z"
                                 child = [] } ] }
          p = Map.ofList [ 1, E.string "b"
                           2, E.string "n"
                           3, E.number "1"
                           4, E.listWith E.int [ 12 ]
                           5, E.unit ()
                           6, E.propertyList [ "low", E.bool true ] ] }
    let objects = [| objectA; objectB |]
