(*
 * Copyright (c) 2019 Frédéric Bour
 *
 * SPDX-License-Identifier: MIT
 *)

open MenhirSdk.Cmly_api

module type RECOVERY = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int

  type recovery = {
    cases: (G.lr1 option * item list) list;
  }
  (* [cases] is a mapping that associates to each possible parent
     (None means for all the other parents) a list of reductions to execute.

     The actual list of actions to reduce an item [(state, prod, pos)] is
     given by
      [Synthesizer.solution (Trail (state, prod, pos))] *)

  val recover : G.lr1 -> recovery
end

module type RECOVER =
  functor (G : GRAMMAR) (S : Synthesis.SYNTHESIZER with module G := G) ->
    (RECOVERY with module G := G)
