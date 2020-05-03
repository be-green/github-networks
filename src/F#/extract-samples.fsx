#time
open System
open System.IO
open System.IO.Compression
open System.Net

Environment.CurrentDirectory <- 
    DirectoryInfo(__SOURCE_DIRECTORY__).Parent.Parent.FullName
let wd = Environment.CurrentDirectory

let openFolder path = 
    Diagnostics.Process.Start("explorer.exe", DirectoryInfo(path).FullName) 
    |> ignore

let makeGhRequestStr d = "https://data.gharchive.org/" + d + ".json.gz"

let decompressGz (fileToDecompress : FileInfo) = 
    use originalFileStream = fileToDecompress.OpenRead()
    let currentFileName = fileToDecompress.FullName
    let newFileName = currentFileName.Remove(currentFileName.Length - fileToDecompress.Extension.Length)
    use decompressedFileStream = File.Create(newFileName)
    use decompressionStream = new GZipStream(originalFileStream, CompressionMode.Decompress)
    do decompressionStream.CopyTo(decompressedFileStream)

let downloadGhArchive pathStr y m d h =
    // make the url query string
    let queryDateString = (DateTime(y,m,d,h,0,0).ToString("yyyy-MM-dd-H"))
    let ghDateString =  makeGhRequestStr queryDateString
    // build the directory/file
    let tempPath =  
        let str = 
            match pathStr with 
            | null | "" -> Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())
            | path -> path
        Directory.CreateDirectory(str).FullName
    
    let compressedFileName = queryDateString + ".json.gz"
    let compressedFilePath = Path.Combine(tempPath, compressedFileName )
    // download the file
    use client = new WebClient()
    client.DownloadFile(ghDateString, compressedFilePath)
    // decompress the file
    do compressedFilePath |> FileInfo |> decompressGz
    // delete compressed file
    File.Delete(Path.Combine(tempPath,compressedFileName))
    
let downloadGhArchiveAll pathStr y m d = 
    // the all day json request doesn't seem to work
    // e.g. https://data.gharchive.org/2015-01-01-{0..23}.json.gz
    // see https://www.gharchive.org/
    for h in 0 .. 23 do downloadGhArchive pathStr y m d h
    // takes ~80 seconds to complete a day

// showing how it works
let samplePath = Path.Combine(wd, "data","archive")
do downloadGhArchiveAll samplePath 2019 2 14
do openFolder samplePath















