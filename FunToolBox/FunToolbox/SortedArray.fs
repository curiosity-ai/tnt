namespace FunToolbox

type 'v sarray =
    | SortedArray of 'v array

    member this.length = let (SortedArray x) = this in x.Length

module SortedArray =

    open System

    /// Converts a sequence to a SortedArray
    let ofSeq s =
        s |> Seq.toArray |> Array.sort |> SortedArray

    /// Converts a SortedArray to an array
    let toArray (sa: 'v sarray) =
        let (SortedArray a) = sa
        a

    /// Converts a SortedArray to a list
    let toList (sa: 'v sarray) = sa |> toArray |> Array.toList

    /// Converts a SortedArray to a sequence
    let toSeq (sa: 'v sarray) = sa |> toArray |> Array.toSeq

    /// Returns the lower bound (the index of the first value that matches the value in the array, or the length of the
    /// array if none found)
    let lowerBound low (sa: 'v sarray) =
        let array = sa |> toArray
        let first = Array.BinarySearch(array, low)

        match first with
        | 0 -> first
        | first when first > 0 ->
            // exact match, find backwards the first one that does not match.
            match Array.FindLastIndex(array, first - 1, Predicate<_>((<>) low)) with
            | -1 -> 0
            | i -> i + 1
        | _ -> ~~~first

    /// Returns the upper bound (the index of the next larger value, or the length of the
    /// array if none found)
    let upperBound high (sa: 'v sarray) =
        let array = sa |> toArray
        let last = Array.BinarySearch(array, high)

        match last with
        | last when last >= 0 ->
            match Array.FindIndex(array, last + 1, Predicate<_>((<>) high)) with
            | -1 -> array.Length
            | i -> i
        | _ -> ~~~last

    /// Returns the sorted sequence range of values from low to high (including)
    let range (low: 'v, high: 'v) (sa: 'v sarray) : 'v seq =
        let lower = lowerBound low sa
        let upper = upperBound high sa
        let a = sa |> toArray
        seq { lower .. upper - 1 } |> Seq.map (fun i -> a[i])

    /// Returns Some index if the value is in the array, otherwise None
    let indexOf (value: 'v) (sa: 'v sarray) : int option =
        let r = Array.BinarySearch(sa |> toArray, value)
        if r >= 0 then Some r else None

    /// Returns true if the value is in the array, otherwise false
    let contains (value: 'v) (sa: 'v sarray) : bool =
        Array.BinarySearch(sa |> toArray, value) >= 0

    let length (sa: 'v sarray) : int = sa.length
