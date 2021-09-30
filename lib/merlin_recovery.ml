(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0

 * Copyright (c) 2019 Frédéric Bour
 * SPDX-License-Identifier: MIT
 *)

module Location = Custom_compiler_libs.Location

let split_pos {Lexing. pos_lnum; pos_bol; pos_cnum; _} =
  (pos_lnum, pos_cnum - pos_bol)

let rev_filter ~f xs =
  let rec aux f acc = function
    | x :: xs when f x -> aux f (x :: acc) xs
    | _ :: xs -> aux f acc xs
    | [] -> acc
  in
  aux f [] xs

let rec rev_scan_left acc ~f ~init = function
  | [] -> acc
  | x :: xs ->
    let init = f init x in
    rev_scan_left (init :: acc) ~f ~init xs

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

module MakePrinter (U : USER_PRINTER)
       : PRINTER with module I = U.I
  =
  struct
    include U
    include MenhirLib.Printers.Make (U.I) (U)
  end

(* Simple printer that do nothing. Useful if debug isn't used *)

module DummyPrinter (I: MenhirLib.IncrementalEngine.EVERYTHING)
       : PRINTER with module I = I
  = MakePrinter (struct
                   module I = I
                   let print _ = ()
                   let print_symbol _ = ()
                   let print_element = None
                   let print_token _ = ()
                 end)

module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : Location.t -> 'a Parser.symbol -> 'a

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

       val recover : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token

       val nullable : 'a Parser.nonterminal -> bool
     end)
    (Printer : PRINTER with module I = Parser) =
struct

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

  module T = struct
    [@@@ocaml.warning "-37"]

    type 'a checkpoint =
      | InputNeeded of 'a Parser.env
      | Shifting of 'a Parser.env * 'a Parser.env * bool
      | AboutToReduce of 'a Parser.env * Parser.production
      | HandlingError of 'a Parser.env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a Parser.checkpoint = "%identity"
  end

  let feed_token ~allow_reduction token env =
    let rec aux allow_reduction = function
      | Parser.HandlingError _ | Parser.Rejected -> `Fail
      | Parser.AboutToReduce _ when not allow_reduction -> `Fail
      | Parser.Accepted v -> `Accept v
      | Parser.Shifting _ | Parser.AboutToReduce _ as checkpoint ->
        aux true (Parser.resume checkpoint)
      | Parser.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
    in
    aux allow_reduction (Parser.offer (T.inj (T.InputNeeded env)) token)

  let rec follow_guide col env = match Parser.top env with
    | None -> col
    | Some (Parser.Element (state, _, pos, _)) ->
      if Recovery.guide (Parser.incoming_symbol state) then
        match Parser.pop env with
        | None -> col
        | Some env -> follow_guide (snd (split_pos pos)) env
      else
        col

  let candidate env =
    let line, min_col, max_col =
      match Parser.top env with
      | None -> 1, 0, 0
      | Some (Parser.Element (state, _, pos, _)) ->
        let depth = Recovery.depth.(Parser.number state) in
        let line, col = split_pos pos in
        if depth = 0 then
          line, col, col
        else
          let col' = match Parser.pop_many depth env with
            | None -> max_int
            | Some env ->
              match Parser.top env with
              | None -> max_int
              | Some (Parser.Element (_, _, pos, _)) ->
                follow_guide (snd (split_pos pos)) env
          in
          line, min col col', max col col'
    in
    { line; min_col; max_col; env }



  let attempt (r : 'a candidates) token =
    let module P = struct
            open Printer

            let num = ref 0

            let print_prelude token =
              match token with (token, _, _) ->
                print ">>>> Recovery attempt on token \"";
                print_token token;
                print "\"\n";

                (* print "Stack:"; *)
                (* print_env r; *)
                print "\n\n"

            let print_candidate x =
              print @@ Printf.sprintf "Candidate #%d\n" !num; num := !num + 1;
              print "Stack:\n";
              print_env x.env

            let print_fail () =
              print ">>>> Recovery failed\n"

            let print_recovered env candidates =
              print ">>>> recovered with state:\n";
              print_current_state env;
              print "\n";
              print "Other candidates:\n";
              List.iteri (fun i c ->
                      print @@ Printf.sprintf "%d: " i;
                      print_current_state c.env;
                      print "\n"
                  ) candidates;

        end
    in
    P.print_prelude token;
    let _, startp, _ = token in
    let line, col = split_pos startp in
    let more_indented candidate =
      line <> candidate.line && candidate.min_col > col in
    let recoveries =
      let rec aux = function
        | x :: xs when more_indented x -> aux xs
        | xs -> xs
      in
      aux r.candidates
    in
    let same_indented candidate =
      line = candidate.line ||
      (candidate.min_col <= col && col <= candidate.max_col)
    in
    let recoveries =
      let rec aux = function
        | x :: xs when same_indented x -> x :: aux xs
        | _ -> []
      in
      aux recoveries
    in
    let rec aux = function
      | [] -> P.print_fail ();
              `Fail
      | x :: xs ->
         P.print_candidate x;
         match feed_token ~allow_reduction:true token x.env with
        | `Fail ->
          aux xs
        | `Recovered (InputNeeded env as checkpoint, _) ->
           P.print_recovered env xs;
           `Ok (checkpoint, x.env)
        | `Recovered _ -> failwith "Impossible"
        | `Accept v ->
          begin match aux xs with
            | `Fail -> `Accept v
            | x -> x
          end
    in
    aux recoveries

  let decide env =
    let rec nth_state env n =
      if n = 0 then
        match Parser.top env with
        | None -> -1 (*allow giving up recovery on empty files*)
        | Some (Parser.Element (state, _, _, _)) -> Parser.number state
      else
        match Parser.pop env with
        | None -> assert (n = 1); -1
        | Some env -> nth_state env (n - 1)
    in
    let st = nth_state env 0 in
    match Recovery.recover st with
    | Recovery.Nothing -> []
    | Recovery.One actions -> actions
    | Recovery.Select f -> f (nth_state env Recovery.depth.(st))

  let generate (type a) (env : a Parser.env) =
    let module E = struct
      exception Result of a
    end in
    let shifted = ref None in
    let rec aux acc env =
      match Parser.top env with
      | None -> None, acc
      | Some (Parser.Element (_state, _, _startp, endp)) ->
        let actions = decide env in
        let candidate0 = candidate env in
        let rec eval (env : a Parser.env) : Recovery.action -> a Parser.env = function
          | Recovery.Abort ->
            raise Not_found
          | Recovery.R prod ->
            let prod = Parser.find_production prod in
            Parser.force_reduction prod env
          | Recovery.S (Parser.N n as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None && not (Recovery.nullable n) then
              shifted := Some xsym;
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            Parser.feed sym endp v endp env
          | Recovery.S (Parser.T t as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None then shifted := Some xsym;
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            let token = (Recovery.token_of_terminal t v, endp, endp) in
            begin match feed_token ~allow_reduction:true token env with
              | `Fail -> assert false
              | `Accept v -> raise (E.Result v)
              | `Recovered (_,env) -> env
            end
          | Recovery.Sub actions ->
            List.fold_left eval env actions
        in
        match
          rev_scan_left [] ~f:eval ~init:env actions
          |> List.map (fun env -> {candidate0 with env})
        with
        | exception Not_found -> None, acc
        | exception (E.Result v) -> Some v, acc
        | [] -> None, acc
        | (candidate :: _) as candidates ->
          aux (candidates @ acc) candidate.env
    in
    let final, candidates = aux [] env in
    (!shifted, final, candidates)

  let generate env =
    Printer.print "Generate candidates for env:\nStack:\n";
    Printer.print_env env;
    let shifted, final, candidates = generate env in
    let candidates = rev_filter candidates
        ~f:(fun t -> not (Parser.env_has_default_reduction t.env))
    in
    { shifted; final; candidates = (candidate env) :: candidates }

end
