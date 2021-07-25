#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore
dotnet tool restore

# Build
dotnet restore
#msbuild /t:Build /p:Configuration=Debug 
dotnet build -c Release

# Gen docs
dotnet run --project ./docsrc/tool
# In order to release, append "ReleaseDocs" when running the command

# dotnet  fsdocs "build" "--input" "docsrc/content/" "--output" "/Users/mathieu/src/fs/Fleece/docsrc/tool/../../docs" "--sourcerepo" "https://github.com/fsprojects/Fleece/tree/master" "--parameters" "root" "/Fleece/" "project-name" "Fleece" "project-author" "Mauricio Scheffer,Lev Gorodinski,Oskar Gewalli, Gustavo P. Leon" "project-summary" "Fleece is a JSON mapper for F#. It simplifies mapping from a Json library's JsonValue onto your types, and mapping from your types onto JsonValue." "project-github" "https://github.com/fsprojects/Fleece" "project-nuget" "http://nuget.org/packages/Fleece"