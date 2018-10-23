﻿module TNT.Library.API

open System
open System.Text
open System.Runtime.CompilerServices
open FunToolbox.FileSystem
open TNT.Model
open TNT.Library.Output

module TranslationGroup = 

    let errorString = function
        | TranslationGroup.AssemblyPathsWithTheSameFilename l ->
            l |> Seq.map ^ fun (fn, paths) ->
                E ^ sprintf "same filename, but different paths: '%s' : %A" (string fn) paths
        | TranslationGroup.TranslationsWithTheSameLanguage l ->
            l |> Seq.map ^ fun ((fn, language), _) ->
                E ^ sprintf "multiple translations of the same language: '%s' of '%s'" (string language)  (string fn)

let extract (assemblyPath: AssemblyPath) : string list output = output {
    let strings = StringExtractor.extract assemblyPath
    yield I ^ sprintf "extracted %d strings from '%s'" (strings.Length) (string assemblyPath)
    return strings
}

type ResultCode =
    | Failed
    | Succeeded

let createNewLanguage (assembly: AssemblyInfo) (language: LanguageIdentifier) : unit output = output {
    let assemblyFilename = assembly.Path |> AssemblyFilename.ofPath
    yield I ^ sprintf "found no language '%s' for '%s', adding one" (string language) (string assemblyFilename)
    let! strings = extract assembly.Path
    let translation = Translation.createNew assembly language strings
    let translationPath = 
        translation |> Translation.path (Directory.current())
    translation |> Translation.save translationPath
    yield I ^ sprintf "new translation saved to '%s'" (string (Path.name translationPath))
}

let private loadGroups() : Result<TranslationGroup, ResultCode> output = output {
    let currentDirectory = Directory.current()
    let translations = Translations.loadAll currentDirectory
    let group = TranslationGroup.fromTranslations translations
    match group with
    | Error(error) ->
        yield! TranslationGroup.errorString error
        return Error(Failed)
    | Ok(group) ->
        return Ok(group)
}

/// tbd: funtoolbox candidate. (also Seq & list)
module Array = 
    let (|IsEmpty|IsNotEmpty|) (array: 'e array) =
        match array with
        | [||] -> IsEmpty
        | _ -> IsNotEmpty array

let status() : ResultCode output = output {
    let! groups = loadGroups()
    match groups with
    | Error(code) ->
        return code
    | Ok(group) ->

    let translations = TranslationGroup.translations group
    let keys = 
        translations 
        |> Seq.map ^ 
            fun translation ->
                let (TranslationId(filename, lang)) = Translation.id translation
                let status = TranslationStatus.ofTranslation translation
                (lang, filename), (status, translation.Assembly.Path)
        |> Seq.sortBy fst
        |> Seq.toArray

    match keys with
    | Array.IsEmpty -> 
        yield I ^ "No translations found"
    | keys ->
        for (lang, filename), (status, path) in keys do
            yield I ^ sprintf "[%s:%s][%s] %s" (string lang) (string filename) (string status) (string path)

    return Succeeded
}

let add (language: LanguageIdentifier) (assemblyLanguage: LanguageIdentifier option, assemblyPath: AssemblyPath option) : ResultCode output = output {
    let currentDirectory = Directory.current()
    let translations = Translations.loadAll currentDirectory
    let group = TranslationGroup.fromTranslations translations
    match group with
    | Error(error) ->
        yield! TranslationGroup.errorString error
        return Failed
    | Ok(group) ->

    match assemblyPath with
    | Some assemblyPath -> 
        let assemblyFilename = assemblyPath |> AssemblyFilename.ofPath
        let set = group |> TranslationGroup.set assemblyFilename
        match set with
        | None -> 
            let assembly = { 
                Language = defaultArg assemblyLanguage (LanguageIdentifier "en-US")
                Path = assemblyPath 
            }
            do! createNewLanguage assembly language
            return Succeeded
        | Some set ->
            let setAssembly = TranslationSet.assembly set
            if setAssembly.Path <> assemblyPath then
                yield E ^ sprintf "assembly path '%s' in the translations files does not match '%s'" (string setAssembly.Path) (string assemblyPath)
                return Succeeded
            else
            match set |> TranslationSet.translation language with
            | Some _ ->
                yield W ^ sprintf "language '%s' already exists for '%s', doing nothing" (string language) (string assemblyFilename)
                return Failed
            | None ->
                do! createNewLanguage setAssembly language
                return Succeeded
    | None ->
        yield E "Adding a language to all available assemblies is unsupported yet"
        return Failed
}        

let update (assembly: AssemblyPath option) = output {
    yield E "not supported"
    return Failed
}

let export 
    (sourceLanguage: LanguageIdentifier) 
    (baseName: XLIFFBaseName)
    (outputDirectory: Path) 
    : ResultCode output = output {
    let currentDirectory = Directory.current()
    let group = TranslationGroup.load currentDirectory
    match group with
    | Error(error) ->
        yield! TranslationGroup.errorString error
        return Failed
    | Ok(group) ->
    let allExports = 
        group
        |> TranslationGroup.translations
        |> List.groupBy ^ fun t -> t.Language
        |> Seq.map ^ fun (language, translations) -> 
            let path = 
                baseName
                |> XLIFFBaseName.filePathForLanguage language outputDirectory 
            let files = ImportExport.export translations
            path, XLIFF.generateV12 sourceLanguage files

    let existingOnes = 
        allExports
        |> Seq.map fst
        |> Seq.filter File.exists
        |> Seq.toList

    if existingOnes <> [] then
        yield E ^ sprintf "one or more exported files already exists, please remove them"
        for existingFile in existingOnes do
            yield E ^ sprintf "  %s" (string existingFile)
        return Failed
    else

    for (file, content) in allExports do
        yield I ^ sprintf "exporting language '%s' to '%s'" (string sourceLanguage) (string file)
        File.saveText Encoding.UTF8 (string content) file

    return Succeeded
}

let import (files: Path list) : ResultCode output = output {
    let currentDirectory = Directory.current()
    let group = TranslationGroup.load currentDirectory
    match group with
    | Error(error) ->
        yield! TranslationGroup.errorString error
        return Failed
    | Ok(group) ->
    let files = 
        files 
        |> Seq.map ^ File.loadText Encoding.UTF8
        |> Seq.map XLIFF.XLIFFV12
        |> Seq.collect XLIFF.parseV12
        |> Seq.toList

    let translations, warnings = 
        let translations = TranslationGroup.translations group
        files 
        |> ImportExport.import translations

    if warnings <> [] then
        yield I ^ "import warnings:"
        for warning in warnings do
            yield W ^ sprintf "  %s" (string warning)

    if translations <> [] then
        yield I ^ sprintf "translations changed and will be updated"
        for translation in translations do
            yield I ^ sprintf "  updating '%s'" (string ^ Translation.filename translation)
            let path = Translation.path currentDirectory translation
            translation |> Translation.save path 
    else
        yield I ^ sprintf "no translations changed"

    return Succeeded
}

[<assembly:InternalsVisibleTo("TNT.Tests")>]
do ()