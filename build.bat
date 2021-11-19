@echo off

cls
dotnet build Fleece.sln -c Release
if ERRORLEVEL 1 (
	echo Error building Fleece
	exit /b 1
) 

dotnet pack src\Fleece -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece
	exit /b 1
)

dotnet pack src\Fleece.SystemJson -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.SystemJson
	exit /b 1
)

dotnet pack src\Fleece.FSharpData -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.FSharpData
	exit /b 1
)

dotnet pack src\Fleece.NewtonsoftJson -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.NewtonsoftJson
	exit /b 1
)

dotnet pack src\Fleece.SystemTextJson -c Release
if ERRORLEVEL 1 (
	echo Error creating package for Fleece.SystemTextJson
	exit /b 1
)
