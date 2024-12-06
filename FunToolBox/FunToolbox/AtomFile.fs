/// Atomic file operations that guarantees that file accesses are atomic as
/// observed from different operating processes on the same machine.
///
/// Note that the functions herein try to reduce the corruption probability
/// in case of power losses to a minimum by flushing the file streams to disk
/// after a write, but ultimately can not guarantee it.
module FunToolbox.AtomFile

open System.IO
open System.Text
open System.Threading

module private Header =
    // we need a header, so we can discriminate between a new file and an empty one.
    let data = "atomfile\n" |> Seq.map (fun c -> c |> byte) |> Seq.toArray
    let len = data.Length

    let is (test: byte[]) =
        test.Length = len
        && Array.compareWith (fun (l: byte) (r: byte) -> l.CompareTo(r)) data test = 0

[<AutoOpen>]
module private Helper =
    let rec openFile fn =
        let ts =
            try
                File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
            with
            // be very specific about the error we tolerate here!
            | :? IOException as e ->
                // 0x80070020: ERROR_SHARING_VIOLATION Windows
                // 0x0000000b: EAGAIN Linux
                if e.HResult = 0x80070020 || e.HResult = 0xb then
                    null
                else
                    reraise ()
        // we can not tail-recurse in exception handlers, so we do that here
        match ts with
        | null ->
            Thread.Sleep(1)
            openFile fn
        | ts -> ts

    let read (bytes: int) (fs: FileStream) : byte[] =
        let a = Array.zeroCreate bytes

        if bytes <> fs.Read(a, 0, bytes) then
            failwith "failed to read from file"

        a

    let write (bytes: byte[]) (fs: FileStream) = fs.Write(bytes, 0, bytes.Length)

    let truncate (fs: FileStream) = fs.SetLength(0 |> int64)

    let getData (fs: FileStream) =
        let l = fs.Length
        let li = l |> int

        if li = 0 then
            None
        else
            let header = fs |> read Header.len

            if Header.is header |> not then
                failwith "failed to read header"

            fs |> read (li - Header.len) |> Some

    let setData (fs: FileStream) (data: byte[] option) =
        fs |> truncate

        if data.IsSome then
            let combined = Array.append Header.data data.Value
            fs |> write combined

        fs.Flush(true)

/// Writes contents to an atomic file. Creates it, if it's not existing.
let write fn data =
    use fs = openFile fn
    setData fs data

/// Reads the contents of a atomic file.
let read fn =
    use fs = openFile fn
    getData fs

type SwapFunction = byte[] option -> byte[] option

/// Atomically exchanges the contents of a file.
let swap (f: SwapFunction) fn =
    use fs = openFile fn
    let input = getData fs

    let output = f input
    output |> setData fs
    output

module Decode =
    let toString (e: Encoding) (value: byte[] option) =
        value |> Option.map (fun b -> e.GetString(b))

module Encode =
    let fromString (e: Encoding) (value: string option) =
        value |> Option.map (fun str -> e.GetBytes(str))

module Swapper =

    /// Convert a string swap function to a byte array swap function.
    let string (e: Encoding) (f: string option -> string option) : (byte[] option -> byte[] option) =
        Decode.toString e >> f >> Encode.fromString e
