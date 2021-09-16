(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0
 *
 * Copyright (c) 2019 Frédéric Bour
 * SPDX-License-Identifier: MIT
 *)

open MenhirSdk.Cmly_api
open Attributes
open Synthesis
open Recovery_intf

module Make
    (G : GRAMMAR)
    (A : ATTRIBUTES with module G := G)
    (S : SYNTHESIZER with module G := G)
    (R : RECOVERY with module G := G) :
sig
  val emit : ?external_tokens:string -> Format.formatter -> unit
end
