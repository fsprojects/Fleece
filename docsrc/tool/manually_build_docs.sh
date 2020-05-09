#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore
dotnet tool restore

# Build
# dotnet build -c Release
msbuild /t:Build /p:Configuration=Release 

# Gen docs
dotnet run --project ./docsrc/tool

