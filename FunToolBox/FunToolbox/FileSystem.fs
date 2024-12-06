module FunToolbox.FileSystem

open System
open System.IO
open Prelude

[<AutoOpen>]
module private Helpers =
    let normalizedPath (path: string) = path.Replace("\\", "/")

    let isEmpty (str: string) = str.Length = 0

    let isTrimmed (str: string) = str.Trim() = str

    let isValidPathCharacter =
        let invalidPathChars = Path.GetInvalidPathChars()
        fun c -> invalidPathChars |> Array.contains c |> not

    let isValidNameCharacter =
        let invalidNameChars = Path.GetInvalidFileNameChars()
        fun c -> invalidNameChars |> Array.contains c |> not

    let isValidPath (path: string) =
        not ^ isEmpty path && isTrimmed path && path |> Seq.forall isValidPathCharacter

    let isValidName (name: string) =
        not ^ isEmpty name && isTrimmed name && name |> Seq.forall isValidNameCharacter

    let isValidAbsolutePath (path: string) =
        isValidPath path && Path.IsPathRooted path

/// Representation of a path. Always an absolute, full, rooted path with normalized
/// Separators (all backslashes converted to forward slashes)
[<Struct>]
type Path =
    | Path of string

    [<Obsolete("use (string path)")>]
    member this.value = string this

    override this.ToString() =
        this
        |> function
            | Path path -> path

/// Representation of a path extension. Always includes the leading period (.).
[<Struct>]
type Ext =
    | Ext of string

    override this.ToString() =
        this
        |> function
            | Ext extension -> extension

module Ext =

    let tryParse str =
        match () with
        | _ when not ^ isValidName str -> Error $"'{str}': extension contains invalid filename characters"
        | _ when not ^ str.startsWith "." -> Ok ^ Ext("." + str)
        | _ -> Ok ^ Ext str

    /// Parses an extension string, must be trimmed and may include a leading period.
    let parse =
        tryParse
        >> function
            | Ok r -> r
            | Error e -> failwith e

module Path =

    let parse str =
        let normalized = str |> normalizedPath

        if not <| isValidAbsolutePath normalized then
            failwith $"'{normalized}' is an invalid absolute path"

        Path normalized

    let map (f: string -> string) (Path path) = path |> f |> parse

    /// Extend the path with a sub-path. The sub-path can be a relative or
    /// absolute path.
    let extend (subPath: string) (path: Path) =
        path |> map ^ fun p -> Path.Combine(p, subPath)

    /// The parent / directory part of the path
    let parent (path: Path) = path |> map Path.GetDirectoryName

    /// Returns the name part of the path.
    let name (Path path) = Path.GetFileName path

    /// Split the path in a directory part and a name part.
    let split (path: Path) = path |> parent, path |> name

    /// Changes the extension of the path. The extension may include the leading period (.).
    /// If the extension is empty, the current extension is removed from the path.
    let withExtension (ext: string) (path: Path) =
        path |> map ^ fun p -> Path.ChangeExtension(p, ext)

    let withExt (Ext ext) (path: Path) =
        path |> map ^ fun p -> Path.ChangeExtension(p, ext)

    let removeExtension path =
        path |> map ^ fun p -> Path.ChangeExtension(p, "")

    let ensureDirectoryExists (Path path) =
        Directory.CreateDirectory path |> ignore

    let ensureDirectoryOfPathExists (path: Path) = path |> parent |> ensureDirectoryExists

    [<Obsolete("use Directory.current")>]
    let currentDirectory () : Path =
        Directory.GetCurrentDirectory() |> parse

    [<Obsolete("use File.exists")>]
    let fileExists (Path path) = File.Exists path

module File =

    let loadBinary (Path path) =
        use stream =
            File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete)

        if stream.Length > (int64 Int32.MaxValue) then
            failwith $"file too long (size: {stream.Length}, max: {Int32.MaxValue})"

        let array = Array.zeroCreate (int stream.Length)
        stream.Read(array, 0, array.Length) |> expect array.Length
        array

    let saveBinary (content: byte[]) (Path path) =
        use stream =
            File.Open(path, FileMode.Create, FileAccess.Write, FileShare.None ||| FileShare.Delete)

        stream.Write(content, 0, content.Length)

    let loadText (encoding: Text.Encoding) (Path path) =
        use stream =
            File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete)

        use reader = new StreamReader(stream, encoding)
        reader.ReadToEnd()

    let saveText (encoding: Text.Encoding) (text: string) (Path path) =
        use stream =
            File.Open(path, FileMode.Create, FileAccess.Write, FileShare.None ||| FileShare.Delete)

        use writer = new StreamWriter(stream, encoding)
        writer.Write(text)

    let loadLines (encoding: Text.Encoding) (Path path) =
        File.ReadAllLines(path, encoding) |> Array.toList

    let exists (Path path) = File.Exists path

    let ensureExists path =
        if not ^ exists path then
            failwith $"file '{path}' does not exist"

    let delete (Path path) = File.Delete(path)

    /// Returns the time the file was last written to in UTC. Throws an exception
    /// if the file does not exist.
    let lastWriteTimeUTC path =
        ensureExists path
        File.GetLastWriteTimeUtc(string path)

    /// Returns the time the file was created in UTC. Throws an exception
    /// if the file does not exist.
    let creationTimeUTC path =
        ensureExists path
        File.GetCreationTimeUtc(string path)

module Directory =

    let current () =
        Directory.GetCurrentDirectory() |> Path.parse

    let exists (Path path) = Directory.Exists(path)

    let deleteIfEmpty (Path path) = Directory.Delete(path)

    let deleteTree (Path path) = Directory.Delete(path, true)
