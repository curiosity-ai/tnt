namespace FunToolbox

/// An atom represents an atomic mutable variable.
/// The basic definition (apply) can only modify a value and return it.
/// All other primitive operations like snapshot or reset implemented in terms of apply.
/// 'v -> 'v is the modification, which returns the resulting value (the 3rd 'v).
type 'v atom = ('v -> 'v) -> 'v

module Atom =

    /// Create a new atom with a thread safe backing store.
    let create v =
        let syncLock = obj ()
        let mutable store = v

        fun f ->
            lock syncLock (fun () ->
                store <- f store
                store)

    /// Apply a function to the atom and return the current snapshot.
    let applyAndSnapshot (f: 'v -> 'v) (a: 'v atom) = a f

    /// Apply a function to the atom.
    let apply (f: 'v -> 'v) (a: 'v atom) = a f |> ignore

    /// Takes something out of the atom.
    let take (f: 'v -> 'v * 'r) (a: 'v atom) =
        let mutable r = Unchecked.defaultof<'r>

        let f s =
            let s, r' = f s
            r <- r'
            s

        apply f a
        r

    /// Return the current value of a Atom.
    let snapshot a = applyAndSnapshot id a

    /// Reset the value to the given one.
    let reset v a : unit = a |> apply (fun _ -> v)

    /// Generate a limited capability to modify a (subset of an existing atom).
    /// This also returns an atom and is similar to a lens.
    let limit (f: 's -> ('n -> 'n) -> 's) (s: 's atom) : 'n atom =
        fun (m: 'n -> 'n) ->
            let mutable c = Unchecked.defaultof<'n>

            let mCapture v =
                c <- m v
                c

            apply (fun s -> f s mCapture) s
            c

    /// Generate a function that returns snapshots of an atom when called.
    let snapshotOnly (a: 's atom) = fun () -> a id
