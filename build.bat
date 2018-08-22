@echo off

cls
msbuild /m /t:restore Fleece.sln
if ERRORLEVEL 1 (
	echo Error restoring packages
	exit /b 1
) 
msbuild /m  Fleece.sln
if ERRORLEVEL 1 (
	echo Error building Fleece
	exit /b 1
) 
dotnet pack Fleece
if ERRORLEVEL 1 (
	echo Error creating package for Fleece
	exit /b 1
)
dotnet pack Fleece.FSharpData
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.FSharpData
	exit /b 1
)
dotnet pack Fleece.NewtonsoftJson
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.NewtonsoftJson
	exit /b 1
)