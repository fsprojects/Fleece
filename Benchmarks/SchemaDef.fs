module JsonSchema

#nowarn "40"

open Fleece

module E = Json.Encode
module D = Json.Decode
module JO = JsonObject

type BoundaryType = Inclusive | Exclusive

module BoundaryType =
    let isExclusive = function
        | Exclusive -> true
        | _ -> false

    let ofExclusiveBool = function
        | true -> Exclusive
        | _ -> Inclusive

type NumericBoundary =
    { limit : decimal
      boundaryType : BoundaryType }

type JsonSchemaType =
    | Object
    | Array
    | String
    | Number
    | Integer
    | Boolean
    | Null

type JsonRegex = JsonRegex of System.Text.RegularExpressions.Regex

type PropertyName = string

type JsonSchemaDefinition =
    { id : System.Uri option
      ``$schema`` : System.Uri option
      title : string option
      description: string option
      ``default`` : Json option
      multipleOf: decimal option
      maximum: NumericBoundary option
      minimum: NumericBoundary option
      maxLength: uint64 option
      minLength: uint64
      pattern: JsonRegex option
      additionalItems: AdditionalElements
      items : ArrayItemsSchema option
      maxItems : uint64 option
      minItems : uint64
      uniqueItems : bool
      maxProperties: uint64 option
      minProperties: uint64
      required: PropertyName list
      additionalProperties: AdditionalElements
      definitions: Map<string, JsonSchema>
      properties: Map<PropertyName, JsonSchema>
      patternProperties: (JsonRegex * JsonSchema) list
      dependencies: Map<PropertyName, Dependencies>
      enum : Json list
      ``type`` : JsonSchemaType list
      allOf: JsonSchema list
      anyOf: JsonSchema list
      oneOf: JsonSchema list
      not : JsonSchema option }
and JsonSchema =
    | EmptySchema
    | SchemaReference of System.Uri
    | Schema of JsonSchemaDefinition
and ArrayItemsSchema =
    | ItemsSchema of JsonSchema
    | TupleSchema of JsonSchema list
and AdditionalElements =
    | Allowed of bool
    | ElementSchema of JsonSchema
and Dependencies =
    | DependentSchema of JsonSchema
    | DependentProperties of PropertyName list

module AdditionalElements =
    let empty = Allowed true

module JsonRegex =
    let get (JsonRegex r) = r
    let pattern (JsonRegex r) = r.ToString()

module JsonSchemaDefinition =
    let empty : JsonSchemaDefinition =
        { id = None; ``$schema`` = None; title = None; description = None
          ``default`` = None; multipleOf = None; maximum = None; minimum = None
          maxLength = None; minLength = 0UL; pattern = None
          additionalItems = AdditionalElements.empty
          items = None; maxItems = None; minItems = 0UL; uniqueItems = false
          maxProperties = None; minProperties = 0UL; required = []
          additionalProperties = AdditionalElements.empty
          definitions = Map.empty
          properties = Map.empty
          patternProperties = []
          dependencies = Map.empty; enum = []; ``type`` = []
          allOf = []; anyOf = []; oneOf = []; not = None }

module Json =
    open Fleece.Operators
    module Encode =
        let uri (u: System.Uri) = E.string (u.ToString())
        let boundaryType (bt: BoundaryType) =
            E.bool (BoundaryType.isExclusive bt)
        let regex (r: JsonRegex) =
            E.string (JsonRegex.pattern r)
        let jsonSchemaType = function
            | JsonSchemaType.Object -> E.string "object"
            | JsonSchemaType.Array -> E.string "array"
            | JsonSchemaType.String -> E.string "string"
            | JsonSchemaType.Number -> E.string "number"
            | JsonSchemaType.Integer -> E.string "integer"
            | JsonSchemaType.Boolean -> E.string "boolean"
            | JsonSchemaType.Null -> E.string "null"
        let jsonSchemaTypeOrTypes = function
            | [jmt] -> jsonSchemaType jmt
            | jmts -> E.listWith jsonSchemaType jmts
        let schemaRef =
            let objectWriter (u: System.Uri) jObj =
                jObj |> E.required uri "$ref" u
            E.buildWith objectWriter
        let rec jsonSchema =
            (do ())
            function
            | EmptySchema -> E.jsonObject JsonObject.empty
            | SchemaReference sr -> schemaRef sr
            | Schema sd -> jsonSchemaDefinitionDelayed () sd
        and regexSchemaList =
            let folder =
                (do ())
                fun (r, s) jObj ->
                    JsonObject.add (JsonRegex.pattern r) (jsonSchema s) jObj
            let objectWriter rsl jObj =
                List.foldBack folder rsl jObj
            E.buildWith objectWriter
        and additionalElements = function
            | Allowed b -> E.bool b
            | ElementSchema s -> jsonSchema s
        and arrayItemsSchema = function
            | ItemsSchema s -> jsonSchema s
            | TupleSchema ms -> E.listWith jsonSchema ms
        and dependencies = function
            | DependentSchema s -> jsonSchema s
            | DependentProperties dp -> E.listWith E.string dp
        and jsonSchemaDefinitionDelayed () =
            let getLimit = (do ()); fun {limit=l} -> l
            let getBoundType = (do ()); fun {boundaryType=bt} -> bt
            let stringList = E.listWith E.string
            let jsonSchemaMap = E.mapWith jsonSchema
            let jsonSchemaList = E.listWith jsonSchema
            let dependenciesMap = E.mapWith dependencies
            let objectWriter =
                fun (jsd: JsonSchemaDefinition) jObj ->
                    jObj
                    |> E.optional uri "id" jsd.id
                    |> E.optional uri "$schema" jsd.``$schema``
                    |> E.optional E.string "title" jsd.title
                    |> E.optional E.string "description" jsd.description
                    |> E.optional E.json "default" jsd.``default``
                    |> E.optional E.decimal "multipleOf" jsd.multipleOf
                    |> E.optional E.decimal "maximum" (jsd.maximum |> Option.map getLimit)
                    |> E.ifNotEqual Inclusive boundaryType "exclusiveMaximum" (jsd.minimum |> Option.map getBoundType |> Option.defaultValue Inclusive)
                    |> E.optional E.decimal "minimum" (jsd.minimum |> Option.map getLimit)
                    |> E.ifNotEqual Inclusive boundaryType "exclusiveMinimum" (jsd.minimum |> Option.map getBoundType |> Option.defaultValue Inclusive)
                    |> E.optional E.uint64 "maxLength" jsd.maxLength
                    |> E.ifNotEqual 0UL E.uint64 "minLength" jsd.minLength
                    |> E.optional regex "pattern" jsd.pattern
                    |> E.ifNotEqual AdditionalElements.empty additionalElements "additionalItems" jsd.additionalItems
                    |> E.optional arrayItemsSchema "items" jsd.items
                    |> E.optional E.uint64 "maxItems" jsd.maxItems
                    |> E.ifNotEqual 0UL E.uint64 "minItems" jsd.minItems
                    |> E.ifNotEqual false E.bool "uniqueItems" jsd.uniqueItems
                    |> E.optional E.uint64 "maxProperties" jsd.maxProperties
                    |> E.ifNotEqual 0UL E.uint64 "minProperties" jsd.minProperties
                    |> E.ifNotEqual [] stringList "required" jsd.required
                    |> E.ifNotEqual AdditionalElements.empty additionalElements "additionalProperties" jsd.additionalProperties
                    |> E.ifNotEqual Map.empty jsonSchemaMap "definitions" jsd.definitions
                    |> E.ifNotEqual Map.empty jsonSchemaMap "properties" jsd.properties
                    |> E.ifNotEqual [] regexSchemaList "patternProperties" jsd.patternProperties
                    |> E.ifNotEqual Map.empty dependenciesMap "dependencies" jsd.dependencies
                    |> E.ifNotEqual [] E.list "enum" jsd.enum
                    |> E.ifNotEqual [] jsonSchemaTypeOrTypes "type" jsd.``type``
                    |> E.ifNotEqual [] jsonSchemaList "allOf" jsd.allOf
                    |> E.ifNotEqual [] jsonSchemaList "anyOf" jsd.anyOf
                    |> E.ifNotEqual [] jsonSchemaList "oneOf" jsd.oneOf
                    |> E.optional jsonSchema "not" jsd.not
            E.buildWith objectWriter
        let jsonSchemaDefinition = jsonSchemaDefinitionDelayed ()

    module Decode =
        let dependent (primaryKey: string) (primaryReader: Decoder<Json,'a>) (dependentKey: string) (dependentReader: Decoder<Json,'b>) : Decoder<JsonObject,('a * 'b option) option> =
            let primaryObjectReader = D.optional primaryReader primaryKey
            let dependentObjectReader = D.optional dependentReader dependentKey
            fun jObj ->
                let aOR = primaryObjectReader jObj
                let bOR = dependentObjectReader jObj
                match aOR, bOR with
                | JPass (Some a), JPass bO ->
                    JsonResult.pass (Some (a, bO))
                | JPass None, JPass (Some _) ->
                    JsonResult.fail (SingleFailure (DeserializationError (typeof<'b>, exn (sprintf "Property '%s' requires property '%s' to be specified" dependentKey primaryKey))))
                | JPass None, JPass None ->
                    JsonResult.pass None
                | JFail err1, JFail err2 ->
                    JsonResult.fail (JsonFailure.mappend err1 err2)
                | JFail err, JPass _
                | JPass _, JFail err ->
                    JsonResult.fail err

        let uri =
            D.string >=> Fleece.Decoder.fromThrowingConverter (fun str -> System.Uri(str, System.UriKind.RelativeOrAbsolute))
        let regexFromString = Fleece.Decoder.fromThrowingConverter (fun str -> JsonRegex <| System.Text.RegularExpressions.Regex (str, System.Text.RegularExpressions.RegexOptions.ECMAScript))
        let regex =
            D.string >=> regexFromString
        let boundaryType =
            D.bool >-> BoundaryType.ofExclusiveBool
        let jsonSchemaType =
            D.string >=> (function
                | "object" -> JsonResult.pass JsonSchemaType.Object
                | "array" -> JsonResult.pass JsonSchemaType.Array
                | "string" -> JsonResult.pass JsonSchemaType.String
                | "number" -> JsonResult.pass JsonSchemaType.Number
                | "integer" -> JsonResult.pass JsonSchemaType.Integer
                | "boolean" -> JsonResult.pass JsonSchemaType.Boolean
                | "null" -> JsonResult.pass JsonSchemaType.Null
                | _ -> JsonResult.deserializationError (exn "Invalid JSON type; must be one of: object, array, number, integer, boolean, string, null"))
        let jsonSchemaTypeOrTypes =
            D.either
                (D.listWith jsonSchemaType)
                (jsonSchemaType >-> fun s -> [s])
        let jsonSchemaRef =
            D.jsonObject >=> D.required uri "$ref"
        let rec jsonSchema =
            D.either
                (jsonSchemaRef >-> SchemaReference)
                (D.lazily jsonSchemaDefinitionDelayed >-> Schema)
        and regexSchemaList =
            D.propertyListWithCustomKey regexFromString jsonSchema
        and additionalElements =
            D.either
                (D.bool >-> Allowed)
                (jsonSchema >-> ElementSchema)
        and arrayItemsSchema =
            D.either
                (jsonSchema >-> ItemsSchema)
                (D.listWith jsonSchema >-> TupleSchema)
        and dependencies =
            D.either
                (D.listWith D.string >-> DependentProperties)
                (jsonSchema >-> DependentSchema)
        and jsonSchemaDefinitionDelayed = lazy (
            let toBoundaryType =
                Option.map (fun (m, mO) -> { limit = m; boundaryType = mO |> Option.defaultValue Inclusive })
            let readMaximum =
                dependent "maximum" D.decimal "exclusiveMaximum" boundaryType >-> toBoundaryType
            let readMinimum =
                dependent "minimum" D.decimal "exclusiveMinimum" boundaryType >-> toBoundaryType
            let makeJsonSchemaDefinition schemaId schema title description def multipleOf maximum minimum maxLen minLen pattern additionalItems items maxItems minItems uniqueItems maxProps minProps required additionalProps defs props patternProps deps enum memType anyOf allOf oneOf not =
                { id = schemaId
                  ``$schema`` = schema
                  title = title
                  description = description
                  ``default`` = def
                  multipleOf = multipleOf
                  maximum = maximum
                  minimum = minimum
                  maxLength = maxLen
                  minLength = minLen
                  pattern = pattern
                  additionalItems = additionalItems
                  items = items
                  maxItems = maxItems
                  minItems = minItems
                  uniqueItems = uniqueItems
                  maxProperties = maxProps
                  minProperties = minProps
                  required = required
                  additionalProperties = additionalProps
                  definitions = defs
                  properties = props
                  patternProperties = patternProps
                  dependencies = deps
                  enum = enum
                  ``type`` = memType
                  allOf = allOf
                  anyOf = anyOf
                  oneOf = oneOf
                  not = not }
            let objectReader =
                makeJsonSchemaDefinition
                <!> D.optional uri "id"
                <*> D.optional uri "$schema"
                <*> D.optional D.string "title"
                <*> D.optional D.string "description"
                <*> D.optional D.json "default"
                <*> D.optional D.decimal "multipleOf"
                <*> readMaximum
                <*> readMinimum
                <*> D.optional D.uint64 "maxLength"
                <*> (D.optional D.uint64 "minLength" >=> D.withDefault 0UL)
                <*> D.optional regex "pattern"
                <*> (D.optional additionalElements "additionalItems" >=> D.withDefault AdditionalElements.empty)
                <*> D.optional arrayItemsSchema "items"
                <*> D.optional D.uint64 "maxItems"
                <*> (D.optional D.uint64 "minItems" >=> D.withDefault 0UL)
                <*> (D.optional D.bool "uniqueItems" >=> D.withDefault false)
                <*> D.optional D.uint64 "maxProperties"
                <*> (D.optional D.uint64 "minProperties" >=> D.withDefault 0UL)
                <*> (D.optional (D.listWith D.string) "required" >=> D.withDefault [])
                <*> (D.optional additionalElements "additionalProperties" >=> D.withDefault AdditionalElements.empty)
                <*> (D.optional (D.mapWith jsonSchema) "definitions" >=> D.withDefault Map.empty)
                <*> (D.optional (D.mapWith jsonSchema) "properties" >=> D.withDefault Map.empty)
                <*> (D.optional regexSchemaList "patternProperties" >=> D.withDefault [])
                <*> (D.optional (D.mapWith dependencies) "dependencies" >=> D.withDefault Map.empty)
                <*> (D.optional D.list "enum" >=> D.withDefault [])
                <*> (D.optional jsonSchemaTypeOrTypes "type" >=> D.withDefault [])
                <*> (D.optional (D.listWith jsonSchema) "allOf" >=> D.withDefault [])
                <*> (D.optional (D.listWith jsonSchema) "anyOf" >=> D.withDefault [])
                <*> (D.optional (D.listWith jsonSchema) "oneOf" >=> D.withDefault [])
                <*> D.optional jsonSchema "not"
            D.jsonObject >=> objectReader )
        let jsonSchemaDefinition = jsonSchemaDefinitionDelayed.Force()

module E = Json.Encode
module D = Json.Decode

type BoundaryType with
    static member FromJson (_: BoundaryType) = D.boundaryType
    static member ToJson x = E.boundaryType x

type JsonSchemaType with
    static member FromJson (_: JsonSchemaType) = D.jsonSchemaType
    static member ToJson x = E.jsonSchemaType x

type JsonRegex with
    static member FromJson (_: JsonRegex) = D.regex
    static member ToJson x = E.regex x

type JsonSchema with
    static member FromJson (_: JsonSchema) = D.jsonSchema
    static member ToJson x = E.jsonSchema x

type ArrayItemsSchema with
    static member FromJson (_: ArrayItemsSchema) = D.arrayItemsSchema
    static member ToJson x = E.arrayItemsSchema x

type AdditionalElements with
    static member FromJson (_: AdditionalElements) = D.additionalElements
    static member ToJson x = E.additionalElements x

type Dependencies with
    static member FromJson (_:Dependencies) = D.dependencies
    static member ToJson x = E.dependencies x

type JsonSchemaDefinition with
    static member FromJson (_:JsonSchemaDefinition) = D.jsonSchemaDefinition
    static member ToJson x = E.jsonSchemaDefinition x

let jsonSchemaStr : string = loadJsonResourceAsString "swagger-schema"
let parsedJson = Json.parse jsonSchemaStr |> JsonResult.getOrThrow
let parsedSchema = D.jsonSchemaDefinition parsedJson |> JsonResult.getOrThrow

open BenchmarkDotNet.Attributes

[<Config(typeof<CoreConfig>)>]
type SwaggerSchema() =
    [<Setup>]
    member __.Setup() =
        // printfn "%A" parsedSchema
        // printfn "%s" (parsedSchema |> E.jsonSchemaDefinition |> Json.format)
        ()

    [<Benchmark>]
    member __.Parse() =
        Json.parse jsonSchemaStr

    [<Benchmark>]
    member __.Decode() =
        D.jsonSchemaDefinition parsedJson

    [<Benchmark>]
    member __.ParseAndDecode() =
        Json.parse jsonSchemaStr
        |> JsonResult.bind D.jsonSchemaDefinition

    [<Benchmark>]
    member __.Format() =
        Json.format parsedJson

    [<Benchmark>]
    member __.Encode() =
        E.jsonSchemaDefinition parsedSchema

    [<Benchmark>]
    member __.EncodeAndFormat() =
        E.jsonSchemaDefinition parsedSchema
        |> Json.format

    [<Benchmark>]
    member __.RoundTrip() =
        Json.parse jsonSchemaStr
        |> JsonResult.bind D.jsonSchemaDefinition
        |> JsonResult.getOrThrow
        |> E.jsonSchemaDefinition
        |> Json.format
