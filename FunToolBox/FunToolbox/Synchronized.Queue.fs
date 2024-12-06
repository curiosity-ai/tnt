module FunToolbox.Synchronized

open System.Threading
open System

/// An unbounded, synchronized queue, that blocks by default when no items
/// are in the queue.
/// (duplicated from the IVR project (Threading.SynchronizedQueue<_>))
type Queue<'item>() =
    let monitor = obj ()
    let queue = System.Collections.Generic.Queue<'item>()

    /// Enqueues one element.
    member this.Enqueue v =
        lock monitor (fun () ->
            queue.Enqueue v

            if queue.Count = 1 then
                Monitor.Pulse monitor)

    /// Dequeues one element, blocks until until at least one item is available.
    member this.Dequeue() =
        lock monitor (fun () ->
            while queue.Count = 0 do
                Monitor.Wait monitor |> ignore

            queue.Dequeue())

    /// Enqueues multiple elements.
    member this.EnqueueAll(list: 'item list) =
        match list with
        | [] -> ()
        | _ ->
            lock monitor (fun () ->
                let pulse = queue.Count = 0
                list |> List.iter queue.Enqueue

                if pulse then
                    Monitor.Pulse monitor)

    /// Dequeues all the items in the queue. Blocks until at least one item is available.
    member this.DequeueAll() =
        lock monitor (fun () ->
            while queue.Count = 0 do
                Monitor.Wait monitor |> ignore

            let r = queue |> List.ofSeq
            queue.Clear()
            r)

    /// Dequeues all the items from the queue. Never blocks and returns an empty list of no items are available.
    member this.DequeueAllNoWait() =
        lock monitor (fun () ->
            let r = queue |> List.ofSeq
            queue.Clear()
            r)

    /// Tries to dequeue an item from the queue. Never blocks. Returns None if no item is available.
    member this.TryDequeue() =
        lock monitor (fun () ->
            match queue.Count with
            | 0 -> None
            | _ -> Some <| queue.Dequeue())

    [<Obsolete("Use Enqueue")>]
    member this.enqueue v = this.Enqueue v

    [<Obsolete("Use Dequeue")>]
    member this.dequeue() = this.Dequeue()
