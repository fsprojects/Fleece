#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
dotnet tool restore
mkdir -p ./bin
mkdir -p ./packages/docs/
NUGET="./bin/nuget.exe"
if test ! -f "$NUGET"; then
  curl -o ./bin/nuget.exe https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
fi
if test "$OS" = "Windows_NT"; then
  MONO=""
else
  MONO="mono"
fi

$MONO $NUGET install FSharp.Compiler.Service "-ExcludeVersion" "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharp.Formatting "-ExcludeVersion" "-version" 9.0.1 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharp.Data "-ExcludeVersion" "-version" 3.0.0 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharpPlus "-ExcludeVersion" "-version" 1.1.1 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install System.Json "-ExcludeVersion" "-version" 4.7.1 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install System.Text.Json "-ExcludeVersion" "-version" 4.7.1 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install Newtonsoft.Json "-ExcludeVersion" "-version" 10.0.2 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
