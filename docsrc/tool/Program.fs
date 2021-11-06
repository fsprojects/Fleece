// Learn more about F# at http://fsharp.org

open System

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
    "fsdocs-logo-src","https://raw.githubusercontent.com/fsprojects/Fleece/master/docsrc/files/img/logo-color.svg"
    "fsdocs-logo-link","http://fsprojects.github.io/Fleece/"
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
open Tools.Path

Target.create "Build" (fun _ ->
    let root = website+"/"
    FSFormatting.buildDocs (fun args ->
        { args with
            OutputDirectory = output
            ProjectParameters =  ("root", root)::info
            Projects = rootDir
            TargetPath = rootDir
            
            SourceRepository = githubLink @@ "tree/master" }
           )
)


// --------------------------------------------------------------------------------------
// Generate the documentation

let root = website

let referenceBinaries = []
open Tools.Path
open System.IO
let bin  = rootDir @@ "src"

let copyFiles () =
    Shell.copyRecursive files output true 
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")

Target.create "Docs" (fun _ ->
    System.IO.File.Delete ( rootDir @@ "docsrc/content/release-notes.md" )
    Shell.copyFile (rootDir @@ "docsrc/content/") "RELEASE_NOTES.md"
    Shell.rename ( rootDir @@ "docsrc/content/release-notes.md" ) "docsrc/content/RELEASE_NOTES.md"

    System.IO.File.Delete ( rootDir @@ "docsrc/content/license.md" )
    Shell.copyFile ( rootDir @@ "docsrc/content/" ) "LICENSE"
    Shell.rename ( rootDir @@ "docsrc/content/license.md" ) "docsrc/content/LICENSE"

    copyFiles ()
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
