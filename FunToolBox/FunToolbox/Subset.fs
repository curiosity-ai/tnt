[<RequireQualifiedAccess>]
module FunToolbox.Subset

/// A type representing a subset mapping in relation to a superset.
type Mapping =
    {
        /// Flags that indicate which entries of the superset exists in the subset.
        Mapping: bool list
    }

/// Select a new subset from a superset.
///
/// - `superset` The superset that has the same length as the original superset from which the subset was created from.
/// - `selector` Chooses the subsets items for the superset's items.
/// Returns the subset's mapping and the subset.
let select (selector: 'i -> 'a option) (superset: 'i list) : Mapping * 'a list =
    let rec collect filter subset todo =
        match todo with
        | [] -> List.rev filter, List.rev subset
        | now :: rest ->
            match selector now with
            | Some r -> collect (true :: filter) (r :: subset) rest
            | None -> collect (false :: filter) subset rest

    let filter, subset = superset |> collect [] []

    { Mapping = filter }, subset

/// Integrate the subset back to _a_ superset.
///
/// The superset do not need to be equal to the original superset that was used to create the subset, but it's length
/// must be the same as the original superset from which the subset got created.
///
/// Use it this way:
/// (superset, subset) |> Subset.integrate mapping (fun (supersetElement, subsetElement option) -> .. )
let integrate (mapping: Mapping) (apply: 'i * 'a option -> 'r) (superset: 'i list, subsetIn: 'a list) : 'r list =

    let rec integrate result filter subset todo =
        match filter, subset, todo with
        | true :: fRest, s :: sRest, input :: rest ->
            let r = apply (input, Some(s))
            integrate (r :: result) fRest sRest rest
        | false :: fRest, subset, input :: rest ->
            let r = (input, None) |> apply
            integrate (r :: result) fRest subset rest
        | [], [], [] -> result |> List.rev
        // Error cases.
        | _ ->
            if List.length superset <> List.length mapping.Mapping then
                failwith "Superset does not match length of original superset the subset was created from"

            if
                List.length subsetIn
                <> (mapping.Mapping |> List.sumBy (fun f -> if f then 1 else 0))
            then
                failwith "Subset does not match length of the original subset"
            else
                failwith "Internal error"

    superset |> integrate [] mapping.Mapping subsetIn
