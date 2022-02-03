(*
 * SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0
 *)

(* This is an explanation of how this algorithm works:

  The problem:
  Given a state. For every parental state return a recovery sequence
  that allows reducing a parental state from the given state in the
  cheapest way.

  Definition:
  State P is parental for state S if S might be reduced on it's items
  to state P.

  Searching for parents:
  First of all, we need to find all possible parents. To do this we
  calculate [depth] which is the largest [pos] among all items.
  It can be proven, that the list of parents is equal to the list of
  states that can reach the given state in [depth] transitions. [pred] is
  the list of states that can reach a given state in one transition.
  Application of [pred] [depth] times returns a list of parents.
  Note: some states have different [pos] among items and the definition of
  parent is not very applicable to them. That is why this algorithm just
  picks the cheapest item to reduce as a recovery sequence. The same happens
  for states without parents.

  The idea:
  We know how to find the cheapest way to reduce an item from a given state.
  It can be done by giving the solver a coresponging [Tail]. After reduction
  automaton reaches the parent and applies transition on nonterminal which is
  the result of the reduction. This transition leads to some states which may
  or may not be a child. In case it is a child we can repeat this step.
  In the opposite case, the automaton reaches the state which has the same item
  as the parent, but the [pos] is increased by one. It means that to reduce
  the parent we can call solver one more time. All of the shift and reduce
  steps we did is a path from the child to a parent reduction. Now we need
  to find the minimal one.

  The graph:
  Let's build an oriented graph. Vertices are nonterminals. Starting
  vertices are nonterminals that are the result of reducing items from the
  given state. Finishing vertices are nonterminals such that the parent
  state has a transition on them to not a child. There is an edge from
  vertice V to U if the resulting state of transition from parental vertice
  on nonterminal V has an item that reduces to nonterminal U. The cost of
  the edge is the cost of such reduction, calculated by solver. Also, we
  have the cost of starting vertices which is equal to the cost of reduction
  to this nonterminal from the given state.

  The algorithm:
  To solve the problem we run Dijkstra's algorithm to calculate the
  cheapest path from any starting vertice to all the finish vertices.
  After that, we add a cost of parent reduction to every finished state and
  pick the cheapest one. This one is the answer to the problem.
  Note: if there are no paths then the algorithm just picks the cheapest
  item to reduce as a recovery sequence as it was done for different [pos]
  states or states without parents.
*)

open MenhirSdk.Cmly_api
open Synthesis

module type RECOVERY = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int

  type recovery = {
    cases: (G.lr1 option * item list) list;
  }

  val recover : G.lr1 -> recovery
end

module Recover (G : GRAMMAR) (S : SYNTHESIZER with module G := G)
  : RECOVERY with module G := G =
struct
  open G
  open Utils

  type item = lr1 * production * int

  type recovery = {
    cases: (G.lr1 option * item list) list;
  }

  type trace = { cost : Cost.t; items : item list }

  module Trace = struct
    type t = trace
    let min tr1 tr2 =
      Cost.arg_min (fun t -> t.cost) tr1 tr2

    let cat tr1 tr2 =
      { cost = Cost.add tr1.cost tr2.cost; items = tr1.items @ tr2.items }
  end

  (* Returns the rightmost [pos] among items *)
  let depth = Lr1.tabulate (fun st ->
    List.fold_left (fun pos (_, pos') -> max pos pos') 0 @@ Lr0.items (Lr1.lr0 st))

  (* Priority queue of nonterminals sorted by the cost of the trace *)
  module State = struct
    module CostNtSet = Set.Make (
      struct
        let compare = compare
        type t = Cost.t * Nonterminal.t
      end)

    (* The set stores tuples of trace cost and nonterminal.
       The array stores traces where index is nonterminal id *)
    type t = CostNtSet.t ref * Trace.t array

    let empty () = (
        ref CostNtSet.empty,
        Array.make Nonterminal.count {cost = Cost.infinite; items = []}
      )

    let is_empty (set, _) = CostNtSet.is_empty !set

    let get_min (set, arr) =
      let (cost, nt) as elem = CostNtSet.min_elt !set in
      let i = Nonterminal.to_int nt in
      let res = nt, arr.(i) in
      set := CostNtSet.remove elem !set;
      res

    let add_elem (set, arr) (nt, tr) =
      let i = Nonterminal.to_int nt in
      let tr' = arr.(i) in
      if tr.cost < tr'.cost then
      begin
        set := CostNtSet.add (tr.cost, nt) @@ CostNtSet.remove (tr'.cost, nt) !set;
        arr.(i) <- tr
      end
  end

  (* For every possible reduction from this states returns a tuple of
     resulting nonterminal and the trace to achive this nonterminal *)
  let synthesize =
    Lr1.tabulate (fun st ->
      List.fold_left (fun acc (prod, pos) ->
        let item = st, prod, pos in
        let cost, _ = S.solve (S.Tail(st, prod, pos)) in
        (Production.lhs prod, {cost = cost; items = [item]}) :: acc
      ) [] (Lr0.items (Lr1.lr0 st)))

  (* The main algorithm *)
  let find_path child parent =
    let module NtTrMap = Map.Make (
      struct
        let compare = compare
        type t = Nonterminal.t
      end)
    in
    (* This is a Map where the key is nonterminal the parent has transition on and
       the value is the chepest trace to make a reduction after this transition *)
    let finish =
      let items = Lr0.items (Lr1.lr0 parent) in
      let traces_to_reduce_parent =
        List.filter_map (fun (prod, pos) ->
          if Array.length (Production.rhs prod) <= pos then
            None
          else
            match (Production.rhs prod).(pos) with
              | (N nt, _, _) ->
                let next = List.assoc (N nt) (Lr1.transitions parent) in
                let cost, _ = S.solve (S.Tail(next, prod, pos + 1)) in
                Some (nt, {cost = cost; items = [next, prod, pos + 1]})
              | _ -> None)
        items
      in
      List.fold_left (fun map (nt, tr) ->
        NtTrMap.update nt (function
          | Some tr' -> if tr'.cost <= tr.cost then Some tr' else Some tr
          | None -> Some tr
        ) map
      ) NtTrMap.empty traces_to_reduce_parent
    in
    let state = State.empty () in
    List.iter (State.add_elem state) @@ synthesize child;
    (* This is the body of Dijkstra's algorithm that picks nonterminal with
       the chepest trace from priority queue and adds new reachable nonterminals to it *)
    let rec dijksta acc =
      if State.is_empty state then
        acc
      else
        let (nt, tr) = State.get_min state in
        match NtTrMap.find_opt nt finish with
          | Some tr' ->
            (* Add trace [tr] to answer with extra trace to reduce parent *)
            dijksta (Trace.cat tr' tr :: acc)
          | _ ->
            match List.assoc_opt (N nt) (Lr1.transitions parent) with
              | None -> dijksta acc (* Corner case *)
              | Some st' ->
                synthesize st'
                |> List.map (fun (nt, tr') -> nt, Trace.cat tr' tr)
                |> List.iter @@ State.add_elem state;
                dijksta acc
    in
    (* Picks the cheapest trace *)
    (List.fold_left Trace.min {cost = Cost.infinite; items = []} @@ dijksta []).items

  (* Returns a list of states that have transition to state [st] *)
  let pred = Lr1.tabulate (fun st ->
    Lr1.fold (fun st' acc ->
      let is_pred st child =
        List.fold_left
        (fun flag (_, st') -> st' = child || flag)
        false (Lr1.transitions st)
      in
      if is_pred st' st then
        st' :: acc
      else
        acc) [])

  (* Returns a list of states that state [st] might be reduced to *)
  let parents st =
    let preds states =
      List.map pred states
      |> List.flatten
      |> List.sort_uniq compare
    in
    let rec aux states = function
      | 0 -> []
      | 1 -> preds states
      | n -> aux (preds states) (n - 1)
    in
    aux [st] (depth st)

  (* Checks if state [st] has items with different position *)
  let has_different_pos st =
    let items = Lr0.items (Lr1.lr0 st) in
    List.length items > 0 &&
    List.length @@ group_assoc @@ List.map (fun (a, b) -> (b, a)) items > 1

  (* Returns the cheapest item to reduce *)
  let cheapest_item st =
    let items = List.map (fun (prod, pos) ->
      let item = st, prod, pos in
      let cost, _ = S.solve (S.Tail(st, prod, pos)) in
      (cost, item)
    ) (Lr0.items (Lr1.lr0 st))
    in
    let (_, cheapest_item) = List.fold_left
      (fun ((costx, (_, _, posx)) as x) ((costy, (_, _, posy)) as y) ->
        if (costx, -posx) < (costy, -posy) then x else y
      ) (List.hd items) items
    in
    cheapest_item

  let recover st : recovery =
    let pars = parents st in
    let cases =
      if has_different_pos st || List.length pars = 0 then
        [(None, [cheapest_item st])]
      else
      List.map (fun parent ->
        match find_path st parent with
        | [] -> (Some parent, [cheapest_item st])
        | xs -> (Some parent, xs)
      ) @@ pars
    in
    {
      cases = cases
    }
end