namespace FleeceB

open BenchmarkDotNet.Attributes
open System.Text

module Constants =
    let short = "hello"
    let medium = String.replicate 1000 "a"
    let mediumWithCommonEscapes = String.replicate 500 "a\n"
    let mediumWithUncommonEscapes = String.replicate 10 ((String.replicate 99 "a") + "\n")
    let mediumWithAllEscapes = String.replicate 1000 "\n"
    let long = String.replicate 1000000 "a"
    let longWithCommonEscapes = String.replicate 500000 "a\n"
    let longWithUncommonEscapes = String.replicate 10000 ((String.replicate 99 "a") + "\n")
    let longWithAllEscapes = String.replicate 1000000 "\n"

[<AutoOpen>]
module Accessory =
    let inline append (s: string) (sb: StringBuilder) =
        sb.Append s |> ignore
    let inline appendChar (c: char) (sb: StringBuilder) =
        sb.Append c |> ignore
    let inline appendChars (cs: char array) (sb: StringBuilder) =
        sb.Append cs |> ignore

    let escapeChars =
        [| '"'; '\\'; '\n'; '\r'; '\t'; '\b'; '\f'
           '\u0000'; '\u0001'; '\u0002'; '\u0003'
           '\u0004'; '\u0005'; '\u0006'; '\u0007'
           '\u000B'; '\u000E'; '\u000F'
           '\u0010'; '\u0011'; '\u0012'; '\u0013'
           '\u0014'; '\u0015'; '\u0016'; '\u0017'
           '\u0018'; '\u0019'; '\u001A'; '\u001B'
           '\u001C'; '\u001D'; '\u001E'; '\u001F' |]

    let isEscapeChar = function
        | '"' | '\\' -> true
        | c when c >= '\u0000' && c <= '\u001F' -> true
        | _ -> false

    let escaped = function
        | '"' -> @"\"""
        | '\\' -> @"\\"
        | '\n' -> @"\n"
        | '\r' -> @"\r"
        | '\t' -> @"\t"
        | '\f' -> @"\f"
        | '\b' -> @"\b"
        | '\u0000' -> @"\u0000"
        | '\u0001' -> @"\u0001"
        | '\u0002' -> @"\u0002"
        | '\u0003' -> @"\u0003"
        | '\u0004' -> @"\u0004"
        | '\u0005' -> @"\u0005"
        | '\u0006' -> @"\u0006"
        | '\u0007' -> @"\u0007"
        | '\u000B' -> @"\u000B"
        | '\u000E' -> @"\u000E"
        | '\u000F' -> @"\u000F"
        | '\u0010' -> @"\u0010"
        | '\u0011' -> @"\u0011"
        | '\u0012' -> @"\u0012"
        | '\u0013' -> @"\u0013"
        | '\u0014' -> @"\u0014"
        | '\u0015' -> @"\u0015"
        | '\u0016' -> @"\u0016"
        | '\u0017' -> @"\u0017"
        | '\u0018' -> @"\u0018"
        | '\u0019' -> @"\u0019"
        | '\u001A' -> @"\u001A"
        | '\u001B' -> @"\u001B"
        | '\u001C' -> @"\u001C"
        | '\u001D' -> @"\u001D"
        | '\u001E' -> @"\u001E"
        | '\u001F' -> @"\u001F"
        | c -> @"\u" + (int c).ToString("X4")

    let rec findFirstEscape (cs:string) (i: int) =
        if i < cs.Length then
            if isEscapeChar cs.[i] then
                i
            else
                findFirstEscape cs (i + 1)
        else
            -1

    let writeStringAlt1 (cs:string) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        let rec inner index =
            if index < cs.Length then
                let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                if nextEscapeIndex = -1 then
                    sb.Append(cs,index,cs.Length - index)
                else if nextEscapeIndex = index then
                    append (escaped cs.[nextEscapeIndex]) sb
                    inner (nextEscapeIndex + 1)
                else
                    sb.Append(cs,index,nextEscapeIndex - index) |> ignore
                    append (escaped cs.[nextEscapeIndex]) sb |> ignore
                    inner (nextEscapeIndex + 1)
            else sb
        inner 0

    let writeStringAlt2 (cs:string) (sb:System.Text.StringBuilder) : System.Text.StringBuilder =
        let rec escapeState index =
            append (escaped cs.[index]) sb |> ignore
            let nextIndex = index + 1
            if nextIndex < cs.Length then
                if isEscapeChar cs.[nextIndex] |> not then
                    coreState nextIndex
                else
                    escapeState nextIndex
            else sb
        and coreState index =
            let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
            if nextEscapeIndex = -1 then
                sb.Append(cs,index,cs.Length - index)
            else
                sb.Append(cs,index,nextEscapeIndex - index) |> ignore
                escapeState nextEscapeIndex
        coreState 0

[<Config(typeof<CoreConfig>)>]
type WriteString () =
    let getString = function
        | "short" -> Constants.short
        | "medium" -> Constants.medium
        | "mediumWithCommonEscapes" -> Constants.mediumWithCommonEscapes
        | "mediumWithUncommonEscapes" -> Constants.mediumWithUncommonEscapes
        | "mediumWithAllEscapes" -> Constants.mediumWithAllEscapes
        | "long" -> Constants.long
        | "longWithCommonEscapes" -> Constants.longWithCommonEscapes
        | "longWithUncommonEscapes" -> Constants.longWithUncommonEscapes
        | "longWithAllEscapes" -> Constants.longWithAllEscapes

    [<Params("short", "medium", "mediumWithCommonEscapes", "mediumWithUncommonEscapes", "mediumWithAllEscapes", "long", "longWithCommonEscapes", "longWithUncommonEscapes", "longWithAllEscapes")>]
    member val String = "" with get, set

    [<Benchmark(Baseline=true)>]
    member x.WriteString () =
        let sb = System.Text.StringBuilder()
        failwith "!"
        //Fleece.Formatting.StringBuilder.writeString sb (getString x.String)

    [<Benchmark>]
    member x.WriteStringAlt1 () =
        let sb = System.Text.StringBuilder()
        writeStringAlt1 (getString x.String) sb

    [<Benchmark>]
    member x.WriteStringAlt2 () =
        let sb = System.Text.StringBuilder()
        writeStringAlt2 (getString x.String) sb
