﻿// Learn more about F# at http://fsharp.org

open System
open Generate

open DocLib


// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docsrc/tools/generate.fsx"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "Fleece"

let website = "/Fleece"

let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

// Specify more information about your project
let info =
  [ "project-name", "Fleece"
    "project-author", "Mauricio Scheffer,Lev Gorodinski,Oskar Gewalli, Gustavo P. Leon"
    "project-summary", "Fleece is a JSON mapper for F#. It simplifies mapping from a Json library's JsonValue onto your types, and mapping from your types onto JsonValue."
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/Fleece" ]

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"


Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs"]
)

let rootDir = __SOURCE_DIRECTORY__ @@ ".." @@ ".."
// --------------------------------------------------------------------------------------
// Build project

Target.create "Build" (fun _ ->
    copyFiles()
    buildDocumentation()
)


// --------------------------------------------------------------------------------------
// Generate the documentation

let root = website

let referenceBinaries = []
open Tools.Path
open System.IO
let bin  = rootDir @@ "src"
let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates; templates @@ "reference"; 
                            formatting @@ "templates"
                            //formatting @@ "templates/reference"
                        ])
Target.create "ReferenceDocs" (fun _ ->
    Directory.ensure (output @@ "reference")
    let nameAndDirectory (d:DirectoryInfo)=
        let net46Bin = DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath (d.FullName @@ "bin" @@ "Release")) |> Array.filter (fun x -> x.FullName.ToLower().Contains("net461"))
        if net46Bin.Length = 0 then failwithf "Failure: No binaries found for %s." d.FullName
        else d.Name, net46Bin.[0]
    let filter (x:DirectoryInfo)= not <| x.FullName.EndsWith("Fleece")
    let binaries () =
        let manuallyAdded = referenceBinaries |> List.map (fun b -> bin @@ b)
   
        let conventionBased = 
            DirectoryInfo.getSubDirectories <| DirectoryInfo bin
            |> Array.filter filter
            |> Array.collect (fun d ->
                let (name, d) = nameAndDirectory d
                d.GetFiles ()
                |> Array.filter (fun x -> x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                |> Array.map (fun x -> x.FullName))
            |> List.ofArray

        conventionBased @ manuallyAdded
    let directories() =
        DirectoryInfo.getSubDirectories <| DirectoryInfo bin
        |> Array.filter filter
        |> Array.map (fun d-> let dir= nameAndDirectory d |> snd in dir.FullName)
        |> List.ofArray

    binaries()
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root)::info
            LibDirs = directories()
            SourceRepository = githubLink @@ "tree/master" }
           )
)

let copyFiles () =
    Shell.copyRecursive files output true 
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Trace.logItems "Copying styles and scripts: "
        
Target.create "Docs" (fun _ ->
    System.IO.File.Delete ( rootDir @@ "docsrc/content/release-notes.md" )
    Shell.copyFile (rootDir @@ "docsrc/content/") "RELEASE_NOTES.md"
    Shell.rename ( rootDir @@ "docsrc/content/release-notes.md" ) "docsrc/content/RELEASE_NOTES.md"

    System.IO.File.Delete ( rootDir @@ "docsrc/content/license.md" )
    Shell.copyFile ( rootDir @@ "docsrc/content/" ) "LICENSE"
    Shell.rename ( rootDir @@ "docsrc/content/license.md" ) "docsrc/content/LICENSE"

    
    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
                    let name = d.Name
                    if name.Length = 2 || name.Length = 3 then
                        layoutRootsAll.Add(
                                name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    copyFiles ()
    
    for dir in  [ content; ] do
        let langSpecificPath(lang, path:string) =
            path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.exists(fun i -> i = lang)
        let layoutRoots =
            let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
            match key with
            | Some lang -> layoutRootsAll.[lang]
            | None -> layoutRootsAll.["en"] // "en" is the default language

        FSFormatting.createDocs (fun args ->
            { args with
                Source = content
                OutputDirectory = output 
                LayoutRoots = layoutRoots
                ProjectParameters  = ("root", root)::info
                Template = docTemplate } )
)

// --------------------------------------------------------------------------------------
// Post process here:



[<EntryPoint>]
let main argv =

    // --------------------------------------------------------------------------------------
    // Release Scripts

    if Array.contains "ReleaseDocs" argv then

        Target.create "ReleaseDocs" (fun _ ->
            let tempDocsDir = rootDir @@ "temp/gh-pages"
            Shell.cleanDir tempDocsDir
            let repoUrl = Git.Config.remoteOriginUrl rootDir
            Git.Repository.cloneSingleBranch rootDir repoUrl "gh-pages" tempDocsDir
            let docDir = rootDir @@ "docs"
            Shell.copyRecursive docDir tempDocsDir true |> Trace.tracefn "%A"
            Git.Staging.stageAll tempDocsDir
            Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
            Git.Branches.push tempDocsDir
        )

        Target.create "GenerateDocs" ignore
    0 // return an integer exit code
