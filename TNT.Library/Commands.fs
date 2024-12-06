﻿module TNT.Library.Commands

open Chiron

module private Names =
    [<Literal>]
    let When = "when"

    [<Literal>]
    let BeforeExtract = "before-extract"

    [<Literal>]
    let Command = "command"

[<Struct; RQA>]
type When =
    | BeforeExtract

    override this.ToString() =
        match this with
        | When.BeforeExtract -> Names.BeforeExtract

module When =
    let tryFromString =
        function
        | Names.BeforeExtract -> Some When.BeforeExtract
        | _ -> None

    let fromString str =
        match tryFromString str with
        | None -> failwithf "Unsupported command trigger '%s'" str
        | Some w -> w

[<Struct>]
type Command = Command of When * string

let serialize (commands: Command list) =

    commands
    |> List.map
       ^ fun (Command(when', cmd)) -> Json.object [ Names.When, string when' |> Json.string; Names.Command, Json.string cmd ]
    |> Seq.toList
    |> Array
    |> Json.formatWith JsonFormattingOptions.Pretty

let deserialize (js: string) =

    let js = Json.parse js

    match js with
    | Array list ->
        let cmd (js: Json) : Command =
            js
            |> Json.destructure
               ^ json {
                   let! trigger = Json.read Names.When
                   let! command = Json.read Names.Command
                   return Command(When.fromString trigger, command)
               }

        list |> List.map cmd

    | _ -> failwith "expect a Json array"

let filter (now: When) (commands: Command list) : string list =
    commands
    |> List.choose ^ fun (Command(w, cmd)) -> if w = now then Some cmd else None
