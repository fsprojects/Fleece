@echo off

cls
if %PROCESSOR_ARCHITECTURE%==x86 (
         set MSBUILD="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
) else ( set MSBUILD="%SystemRoot%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
)
if not exist .nuget\nuget.exe %MSBUILD% .nuget\nuget.targets /t:CheckPrerequisites
%MSBUILD% /m /p:Configuration=SystemJson Fleece.sln
if ERRORLEVEL 1 (
	echo Error building Fleece for System.Json
	exit /b 1
)
%MSBUILD% /m /p:Configuration=FSharpData Fleece.sln
if ERRORLEVEL 1 (
	echo Error building Fleece for FSharp.Data
	exit /b 2
)
