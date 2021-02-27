Push-Location
try {
    $rootDir = [IO.Path]::Combine($PSScriptRoot, ".." , "..")
    Set-Location $rootDir
    dotnet tool restore
    if (!(Test-Path "./bin")) {
        mkdir -p ./bin
    }
    if (!(Test-Path "./packages/docs/")) {
        mkdir -p ./packages/docs/
    }
    $nuget="./bin/nuget.exe"

    if (!(Test-Path $nuget)) {
        Invoke-WebRequest -Uri https://dist.nuget.org/win-x86-commandline/latest/nuget.exe -OutFile ./bin/nuget.exe
    }
    & $nuget install FSharp.Compiler.Service "-ExcludeVersion" "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
    & $nuget install FSharp.Formatting "-ExcludeVersion" "-version" 8.0.1 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/

}
finally {
    Pop-Location
}
