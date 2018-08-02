@echo off

cls
msbuild /m /t:restore /p:Configuration=Benchmarks Fleece.sln
if ERRORLEVEL 1 (
    echo Error restoring packages
    exit /b 1
) 
msbuild /m /t:build /p:Configuration=Benchmarks Fleece.sln
if ERRORLEVEL 1 (
    echo Error building Fleece (in Benchmarks configuration)
    exit /b 1
) 
