namespace FunToolbox.Time

open System
open System.Diagnostics
open FunToolbox.Prelude

[<AutoOpen>]
module private Const =
    let ticksPerSecond = Stopwatch.Frequency |> float

/// High Resolution timestamp that is based on internal CPU timers.
/// Optimized for precise measurements over short periods of time.
[<Struct>]
type Timestamp =
    private
    | Timestamp of int64

    static member Now = Stopwatch.GetTimestamp() |> Timestamp

    static member (-)(Timestamp l, Timestamp r) =
        (l - r |> float) / ticksPerSecond |> TimeSpan.FromSeconds

    static member (+)(Timestamp l, r: TimeSpan) =
        l + ((r.TotalSeconds * ticksPerSecond) |> int64) |> Timestamp

    static member (-)(Timestamp l, r: TimeSpan) =
        l - ((r.TotalSeconds * ticksPerSecond) |> int64) |> Timestamp

    static member op_LessThan(Timestamp l, Timestamp r) = l < r
    static member op_GreaterThan(Timestamp l, Timestamp r) = l > r
    static member op_LessThanOrEqual(Timestamp l, Timestamp r) = l <= r
    static member op_GreaterThanOrEqual(Timestamp l, Timestamp r) = l >= r

[<RQA>]
module Timestamp =
    let now () = Timestamp.Now
