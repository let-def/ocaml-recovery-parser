(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0
 *
 * Copyright (c) 2019 Frédéric Bour
 * SPDX-License-Identifier: MIT
 *)

open MenhirSdk
open Recovery_custom

let name = ref ""
let external_tokens = ref None
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] [--external-tokens Module.Name] file.cmly\n"
    Sys.argv.(0);
  exit 1

let () =
  let i = ref 1 in
  while !i < Array.length Sys.argv do
    begin match Sys.argv.(!i) with
    | "-v" -> verbose := true
    | "--external-tokens" ->
       if (!i+1) = Array.length Sys.argv || !external_tokens <> None
       then usage ()
       else external_tokens := Some Sys.argv.(!i + 1);
            i := !i + 1
    | arg -> if !name = "" then name := arg else usage ()
    end;
    i := !i + 1
  done;
  if !name = "" then
    usage ()

module G = Cmly_read.Read (struct let filename = !name end)
module A = Attributes.Recover_attributes(G)

let () =
  let open Format in
  let ppf = Format.err_formatter in
  if !verbose then begin
    let open G in
    Lr1.iter (fun (st : lr1) ->
        fprintf ppf "\n# LR(1) state #%d\n\n" (st :> int);
        fprintf ppf "Items:\n";
        Print.itemset ppf (Lr0.items (Lr1.lr0 st));
        fprintf ppf "Transitions:\n";
        List.iter (fun (sym,(st' : lr1)) ->
            fprintf ppf " - on %a, goto #%d\n"
              Print.symbol sym
              (st' :> int)
          ) (Lr1.transitions st);
        fprintf ppf "Reductions:\n";
        List.iter (fun (t,ps) ->
            let p : production = List.hd ps in
            fprintf ppf " - on %a, reduce %d:\n  %a\n"
              Print.terminal t
              (p :> int) Print.production p
          ) (Lr1.reductions st);
      );
    Production.iter (fun (p : production) ->
        fprintf ppf "\n# Production p%d\n%a"
          (p :> int) Print.production p
      );
  end

module S = Synthesis.Synthesizer(G)(A)

let () = if !verbose then S.report Format.err_formatter

module R = Recover(G)(S)

(*let () = if !verbose then R.report Format.err_formatter*)

module E = Emitter.Make(G)(A)(S)(R)

let () = E.emit ?external_tokens:!external_tokens Format.std_formatter
