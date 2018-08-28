// Learn more about F# at http://fsharp.org
module FleeceB.Benchmarks.Program

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Diagnosers
//open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Validators
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let switcher = BenchmarkSwitcher thisAssembly
    let _ = switcher.Run argv
    0
