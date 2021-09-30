(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0

 * Copyright (c) 2019 Frédéric Bour
 * SPDX-License-Identifier: MIT
 *)

(* Minimal signature that required from user for debug printing (tracing) *)

module type USER_PRINTER =
  sig
    module I : MenhirLib.IncrementalEngine.EVERYTHING

    val print : string -> unit
    val print_symbol : I.xsymbol -> unit
    val print_element : (I.element -> unit) option
    val print_token : I.token -> unit
  end

(* Full signature
     That separation allows users to redefine functions from full interface *)

module type PRINTER =
  sig
    include USER_PRINTER

    val print_current_state : 'a I.env -> unit
    val print_env : 'a I.env -> unit
  end

(* Make full parser from minimal one *)

module MakePrinter (U : USER_PRINTER) : PRINTER with module I = U.I

(* Simple printer that do nothing. Useful if debug isn't used *)

module DummyPrinter (I: MenhirLib.IncrementalEngine.EVERYTHING)
       : PRINTER with module I = I

module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : Custom_compiler_libs.Location.t -> 'a Parser.symbol -> 'a

       type action =
         | Abort
         | R of int
         | S : 'a Parser.symbol -> action
         | Sub of action list

       type decision =
         | Nothing
         | One of action list
         | Select of (int -> action list)

       val depth : int array

       val can_pop : 'a Parser.terminal -> bool

       val recover : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token

       val nullable : 'a Parser.nonterminal -> bool
     end)
    (Printer : PRINTER with module I = Parser) :
sig

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    shifted: Parser.xsymbol option;
    final: 'a option;
    candidates: 'a candidate list;
  }

  val attempt : 'a candidates ->
    Parser.token * Lexing.position * Lexing.position ->
    [> `Accept of 'a
    | `Fail
    | `Ok of 'a Parser.checkpoint * 'a Parser.env ]

  val generate : 'a Parser.env -> 'a candidates

end
