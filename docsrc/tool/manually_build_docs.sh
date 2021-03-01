#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore
dotnet tool restore

./docsrc/tool/download_nugets.sh
# Build
# dotnet build -c Release
msbuild /t:Build /p:Configuration=Debug 

#cp packages/docs/FSharp.Data/lib/netstandard2.0/FSharp.Data.* /Users/mathieu/src/fs/Fleece/src/Fleece.FSharpData/bin/Debug/netstandard2.0/
#cp packages/docs/FSharpPlus/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.FSharpData/bin/Debug/netstandard2.0/

#cp packages/docs/FSharpPlus/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.NewtonsoftJson/bin/Debug/netstandard2.0/
#cp packages/docs/Newtonsoft.Json/lib/netstandard1.3/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.NewtonsoftJson/bin/Debug/netstandard2.0/

#cp packages/docs/FSharpPlus/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.SystemJson/bin/Debug/netstandard2.0/
#cp packages/docs/System.Json/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.SystemJson/bin/Debug/netstandard2.0/

#cp packages/docs/FSharpPlus/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.SystemTextJson/bin/Debug/netstandard2.0/
#cp packages/docs/System.Text.Json/lib/netstandard2.0/*.* /Users/mathieu/src/fs/Fleece/src/Fleece.SystemTextJson/bin/Debug/netstandard2.0/

# Gen docs
dotnet run --project ./docsrc/tool ReleaseDocs

# dotnet  fsdocs "build" "--input" "docsrc/content/" "--output" "/Users/mathieu/src/fs/Fleece/docsrc/tool/../../docs" "--sourcerepo" "https://github.com/fsprojects/Fleece/tree/master" "--parameters" "root" "/Fleece/" "project-name" "Fleece" "project-author" "Mauricio Scheffer,Lev Gorodinski,Oskar Gewalli, Gustavo P. Leon" "project-summary" "Fleece is a JSON mapper for F#. It simplifies mapping from a Json library's JsonValue onto your types, and mapping from your types onto JsonValue." "project-github" "https://github.com/fsprojects/Fleece" "project-nuget" "http://nuget.org/packages/Fleece"