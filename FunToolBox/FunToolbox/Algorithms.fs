namespace FunToolbox.Algorithms

open System.Linq
open FunToolbox.Prelude
open System.Collections.Generic

/// A directed graph
type Graph<'t> = private Graph of ('t * 't list) list
type 't graph = Graph<'t>

module Graph =

    exception CycleFoundException

    let sortTopologically (Graph graph) =

        // use a dictionary instead of a map, so that we don't need the comparison constraint, and
        // stay linear in performance.
        let lookup = graph.ToDictionary(fst, snd)
        let visited = HashSet<'t>()

        let dfs result start_node =

            // there is only one path when we run a depth first search,
            // so we can optimize this by using a mutable HashSet, instead of Set or a List.
            // This also this avoids the comparison constraint that a Set would introduce.
            let path = HashSet<'t>()

            let rec explore result node =
                if visited.Contains node then
                    result
                else if path.Contains node then
                    raise CycleFoundException
                else
                    path.Add node |> ignore

                    let edges =
                        match lookup.TryGetValue node with
                        | true, edges -> edges
                        | false, _ -> []

                    let result = List.fold explore result edges
                    visited.Add node |> ignore
                    node :: result in

            explore result start_node

        Seq.fold (fun result (node, _) -> dfs result node) [] graph

    let ofEdges (edges: ('t * 't) list) =
        edges
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd)
        |> Graph

    let ofFans (fans: ('t * 't list) list) =
        fans
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.collect snd)
        |> Graph

/// Simple probability selection in the form [(probability: int, value)]
module Probability =
    /// Sum all probabilities in the probability array.
    let sum table = table |> Seq.map fst |> Seq.sum

    /// Returns an index in to the probability table based on a probability value.
    let select probability table =
        let table = table |> Seq.toArray
        assert (probability >= 0 && probability < sum table)
        let highs = table |> Seq.map fst |> Seq.scan (+) 0 |> Seq.skip 1
        let index = highs |> Seq.findIndex ^ fun high -> high > probability
        assert (index < table.Length)
        index
