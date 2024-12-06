﻿namespace TNT.Library

open TNT.Model
open System.Json
open Chiron

module Sources =

    let deserialize (json: string) : Sources =
        let value = JsonValue.Parse json

        let str (value: JsonValue) : string = JsonValue.op_Implicit (value)

        let parseAssemblySource (value: JsonValue) : Source =
            AssemblySource(RPath.parse (str value.["path"]))

        let parseSource (value: JsonArray) : Source =
            if value.Count <> 2 then
                failwithf "a source must have a type and a content object"

            match str value.[0] with
            | "assembly" -> parseAssemblySource value.[1]
            | unknown -> failwithf "unsupported source type: '%s'" unknown


        let language = str value.["language"] |> LanguageTag

        let sources =
            value.["sources"] :?> JsonArray |> Seq.map (unbox >> parseSource) |> Seq.toList

        { Language = language
          Sources = sources |> Set.ofList }

    let serialize (sources: Sources) : string =

        let inline str (v: 'v) = v |> string |> String

        let src name properties =
            Json.array [ String name; Json.object properties ]

        let sourceJson (source: Source) =
            match source with
            | AssemblySource path -> src "assembly" [ "path", str path ]

        Json.object
            [ "language", str sources.Language
              "sources", Json.array (sources.Sources |> Seq.map sourceJson) ]
        |> Json.formatWith JsonFormattingOptions.Pretty

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
