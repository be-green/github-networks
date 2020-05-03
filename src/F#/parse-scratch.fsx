#time
#r @"packages\fsharp.data\3.3.3\lib\net45\FSharp.Data.dll"

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Data.JsonExtensions

Environment.CurrentDirectory <- 
    Directory.GetParent(__SOURCE_DIRECTORY__).Parent.FullName
let wd = Environment.CurrentDirectory

let jsonSample = 
    Path.Combine(wd, "data", "archive", "2019-02-14-23.json")
    |> File.ReadLines

let parseType input = Regex.Match(input, "(?<=\"type\":\")\w*").Value

let groupSizes = 
    jsonSample
    |> Seq.groupBy parseType
    |> Seq.map (fun (a,b) -> (a, Seq.length b))
    |> Seq.sortByDescending snd
    |> Seq.toList

// writing sample payloads
let payloadExamples = 
    groupSizes 
    |> Seq.map fst 
    |> Seq.sort 
    |> Seq.map (fun typ -> Seq.find (fun x -> parseType x = typ) jsonSample)
    |> Seq.map (fun jsonRaw -> 
        let json = JsonValue.Parse(jsonRaw)
        json.["type"].AsString(), json.["payload"])
    |> Map.ofSeq

payloadExamples
|> Map.iter (fun example payload ->
        let outputpath = Path.Combine(wd, "data", example, "-sample-payload.json")
        File.WriteAllText(outputpath, payload.ToString()) )


// parsing = = = = = = = = = = =
//parsing examples
let nfo = JsonValue.Load(Path.Combine(wd,"data","push-samples.json"))
let asd = nfo.[0]
asd.["id"].AsInteger64()
asd?id
asd?payload
asd.Properties
asd?payload
asd?repo
asd?repo.AsArray()
asd.["repo"].AsArray()
[for v in asd?repo -> v.AsString()]
asd?repo.Properties

asd?payload.Properties
asd?payload?commits

match asd?payload?commits with
| JsonValue.Array x -> 0
| JsonValue.Record x -> 1
| _ -> -1

let zxc = asd?payload?commits.[0]
zxc.Properties
zxc?author
asd?payload?commits.AsArray()
// type provider

let showProps json = 
    let rec loop (leads :  seq<string>) (json : JsonValue) = 
        let _printit leads (s : string) marker = 
            Seq.append leads [s]
            |> String.concat "." 
            |> printfn "%15s: %s" marker

        for a,b in json.Properties do
            let printit marker = _printit leads a marker
            match b with
            | JsonValue.Null -> printit "Null"
            | JsonValue.Record _ -> loop (Seq.append leads [a]) b
            | JsonValue.Array x when Array.isEmpty x -> 
                printit "Empty Array"
            | JsonValue.Array _ -> 
                loop (Seq.append leads [a]) b.[0]
            | JsonValue.String _ -> printit "String" 
            | JsonValue.Boolean _ -> printit "Boolean" 
            | JsonValue.Float _ -> printit "Float" 
            | JsonValue.Number _ -> printit "Number"
            
    loop [] json


payloadExamples
|> Map.iter (fun key eg ->
    printfn "\r\n\r\n%s" key
    showProps eg
    )


// how fast is it to parse each one?
jsonSample |> Seq.iter (fun x -> JsonValue.Parse(x) |> ignore)


let flatten (json : JsonValue) = 
    [ for key,value in json.Properties -> props]

payloadExamples |> Map.toSeq |> Seq.map fst |> Seq.toArray
let sampleRecord = payloadExamples.["IssueCommentEvent"]?issue?pull_request
flatten sampleRecord


let toCsv (json : JsonValue) = 
    seq {
        for key, value in json.Properties do
            if key <> "payload" then 
                yield key
    }

toCsv asd
asd.["actor"]

asd.Properties 
|> Array.map fst 

asd.["public"]

let rec test (gh : JsonValue) =
    let output = 
        {|
            Id    = gh?id.AsInteger64()
            Type  = gh.["type"].AsString()
            Actor = gh?actor
            Repo  = gh?repo
            Public = gh.["public"]
            Created = gh?created_at.AsDateTime()
        |}
    output
    //match json with
    //| JsonValue.Record x -> "record"
    //| _ -> "other"
let rec test2 (gh : JsonValue) = 
    match gh with
    | JsonValue.String x -> x
    | _ -> "nope"

test asd
asd

[for (a,b) in asd.Properties -> a]
[for (idx,prop) in asd.Properties -> (idx,test2 prop)]



//type TopLevel = JsonProvider<"C:/Users/jared/source/repos/github-networks/dev/fsharp-parsing/fsharp-parsing/json-samples/top-level-sample.json">

//type PushEvent = JsonProvider<"C:/Users/jared/source/repos/github-networks/dev/fsharp-parsing/fsharp-parsing/json-samples/PushEvent-sample.json">


let pushJson = 
    jsonSample 
    |> Seq.filter (parseType >> ((=)"PushEvent"))

let allPushes = "[ \r\n" + String.Join(",\r\n",pushJson) + "\r\n]"

File.WriteAllText("C:/Temp/push-samples.json",allPushes)

//type PushEvent = JsonProvider<"C:/Users/jared/source/repos/github-networks/dev/fsharp-parsing/fsharp-parsing/json-samples/push-samples.json",SampleIsList = true>

let pushObjects = 
    pushJson
    |> Seq.map PushEvent.Parse
    |> Seq.toList

pushObjects.[0].Payload.Commits

for push in pushObjects |> Seq.take 5 do
    printfn "%A" push.Payload.Ref













let wbReq = 
  "http://api.worldbank.org/country/cz/indicator/" + 
  "GC.DOD.TOTL.GD.ZS?format=json"

let wb = JsonValue.AsyncLoad(wbReq) |> Async.RunSynchronously
let wb = JsonValue.Load(wbReq)
wb.ToString()

wb?page

match wb with
| JsonValue.Array [| info; data |] ->
    // Print overall information
    let page, pages, total = info?page, info?pages, info?total
    printfn 
      "Showing page %d of %d. Total records %d" 
      (page.AsInteger()) (pages.AsInteger()) (total.AsInteger())
    
    // Print every non-null data point
    for record in data do
      if record?value <> JsonValue.Null then
        printfn "%d: %f" (record?date.AsInteger()) 
                         (record?value.AsFloat())
| _ -> printfn "failed"


