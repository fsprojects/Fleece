@echo off

cls
msbuild /m /t:restore Fleece.sln
msbuild /m  Fleece.sln
if ERRORLEVEL 1 (
	echo Error building Fleece
	exit /b 1
) else (
	dotnet pack Fleece
	dotnet pack Fleece.FSharpData
)
