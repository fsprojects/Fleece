@echo off

cls
dotnet build Fleece.sln -c Release
if ERRORLEVEL 1 (
	echo Error building Fleece
	exit /b 1
) 

dotnet pack Fleece -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece
	exit /b 1
)

dotnet pack Fleece.FSharpData -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.FSharpData
	exit /b 1
)

dotnet pack Fleece.NewtonsoftJson -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.NewtonsoftJson
	exit /b 1
)