namespace TNT.Library

open System.Text.Json
open System.Text.Json.Nodes
open TNT.Model
open Chiron

module Sources =

    let deserialize (json: string) : Sources =
        let doc = JsonNode.Parse(json)

        let str (node: JsonNode) : string = node.GetValue<string>()

        let parseAssemblySource (node: JsonNode) : Source =
            AssemblySource(RPath.parse (str node["path"]))

        let parseSource (node: JsonNode) : Source =
            if node.AsArray().Count <> 2 then
                failwithf "a source must have a type and a content object"

            match str node.[0] with
            | "assembly" -> parseAssemblySource node.[1]
            | unknown -> failwithf "unsupported source type: '%s'" unknown

        let language = str doc["language"] |> LanguageTag
        let sources = doc["sources"].AsArray() |> Seq.map parseSource |> Seq.toList

        { Language = language
          Sources = sources |> Set.ofList }

    let serialize (sources: Sources) : string =
        let options = JsonSerializerOptions(WriteIndented = true)

        let sourceToJsonNode (source: Source) =
            match source with
            | AssemblySource path ->
                let arr = JsonArray()
                arr.Add("assembly")
                let props = JsonObject()
                props.Add("path", string path)
                arr.Add(props)
                arr

        let root = JsonObject()
        root.Add("language", string sources.Language)

        let sourcesArray = JsonArray()
        sources.Sources |> Set.toSeq |> Seq.iter (sourceToJsonNode >> sourcesArray.Add)

        root.Add("sources", sourcesArray)

        root.ToJsonString(options)

[<AutoOpen>]
module SourceExtensions =

    type Source with
        member this.Format =
            match this with
            | AssemblySource path -> Format.prop "assembly" (string path)

    type Sources with
        member this.Format =
            [ Format.prop "language" (string this.Language)
              Format.group "sources" (this.Sources |> Set.toList |> List.map ^ fun s -> s.Format) ]
