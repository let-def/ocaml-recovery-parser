(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0

 * Copyright (c) 2019 Frédéric Bour
 * SPDX-License-Identifier: MIT
 *)

module Pprintast = Custom_compiler_libs.Pprintast

(* Lexing *)

let lex_buf lexbuf =
  Raw_lexer.init ();
  let rec loop acc =
    match Raw_lexer.token lexbuf with
    | exception (Raw_lexer.Error _) ->
      (* FIXME: Do something with the error
         (print, remember it, generate special token, whatever :P) *)
      loop acc
    | token ->
      let acc = (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: acc in
      match token with
      | Raw_parser.EOF -> List.rev acc
      | _ -> loop acc
  in
  loop []

let lex_file fname =
  let ic = open_in_bin fname in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  let tokens = lex_buf lexbuf in
  close_in_noerr ic;
  tokens

let string_of_token tok =
  let p name fmt = Printf.ksprintf (fun s -> name ^ " " ^ s) fmt in
  let s name x = p name "%S" x in
  match tok with
  | Raw_parser.AMPERAMPER             -> "AMPERAMPER"
  | Raw_parser.AMPERSAND              -> "AMPERSAND"
  | Raw_parser.AND                    -> "AND"
  | Raw_parser.ANDOP x                -> s "ANDOP" x
  | Raw_parser.AS                     -> "AS"
  | Raw_parser.ASSERT                 -> "ASSERT"
  | Raw_parser.BACKQUOTE              -> "BACKQUOTE"
  | Raw_parser.BANG                   -> "BANG"
  | Raw_parser.BAR                    -> "BAR"
  | Raw_parser.BARBAR                 -> "BARBAR"
  | Raw_parser.BARRBRACKET            -> "BARRBRACKE"
  | Raw_parser.BEGIN                  -> "BEGIN"
  | Raw_parser.CHAR c                 -> p "CHAR" "%C" c
  | Raw_parser.CLASS                  -> "CLASS"
  | Raw_parser.COLON                  -> "COLON"
  | Raw_parser.COLONCOLON             -> "COLONCOLON"
  | Raw_parser.COLONEQUAL             -> "COLONEQUAL"
  | Raw_parser.COLONGREATER           -> "COLONGREAT"
  | Raw_parser.COMMA                  -> "COMMA"
  | Raw_parser.COMMENT (x, _loc)      -> p "COMMENT" "%S" x
  | Raw_parser.CONSTRAINT             -> "CONSTRAINT"
  | Raw_parser.DO                     -> "DO"
  | Raw_parser.DOCSTRING _            -> "DOCSTRING"
  | Raw_parser.DONE                   -> "DONE"
  | Raw_parser.DOT                    -> "DOT"
  | Raw_parser.DOTDOT                 -> "DOTDOT"
  | Raw_parser.DOTOP x                -> s "DOTOP" x
  | Raw_parser.DOWNTO                 -> "DOWNTO"
  | Raw_parser.ELSE                   -> "ELSE"
  | Raw_parser.END                    -> "END"
  | Raw_parser.EOF                    -> "EOF"
  | Raw_parser.EOL                    -> "EOL"
  | Raw_parser.EQUAL                  -> "EQUAL"
  | Raw_parser.EXCEPTION              -> "EXCEPTION"
  | Raw_parser.EXTERNAL               -> "EXTERNAL"
  | Raw_parser.FALSE                  -> "FALSE"
  | Raw_parser.FLOAT (x, _)           -> s "FLOAT" x
  | Raw_parser.FOR                    -> "FOR"
  | Raw_parser.FUN                    -> "FUN"
  | Raw_parser.FUNCTION               -> "FUNCTION"
  | Raw_parser.FUNCTOR                -> "FUNCTOR"
  | Raw_parser.GREATER                -> "GREATER"
  | Raw_parser.GREATERRBRACE          -> "GREATERRBR"
  | Raw_parser.GREATERRBRACKET        -> "GREATERRBR"
  | Raw_parser.HASH                   -> "HASH"
  | Raw_parser.HASHOP x               -> s "HASHOP" x
  | Raw_parser.IF                     -> "IF"
  | Raw_parser.IN                     -> "IN"
  | Raw_parser.INCLUDE                -> "INCLUDE"
  | Raw_parser.INFIXOP0 x             -> s "INFIXOP0" x
  | Raw_parser.INFIXOP1 x             -> s "INFIXOP1" x
  | Raw_parser.INFIXOP2 x             -> s "INFIXOP2" x
  | Raw_parser.INFIXOP3 x             -> s "INFIXOP3" x
  | Raw_parser.INFIXOP4 x             -> s "INFIXOP4" x
  | Raw_parser.INHERIT                -> "INHERIT"
  | Raw_parser.INITIALIZER            -> "INITIALIZE"
  | Raw_parser.INT (x, _)             -> s "INT" x
  | Raw_parser.LABEL x                -> s "LABEL" x
  | Raw_parser.LAZY                   -> "LAZY"
  | Raw_parser.LBRACE                 -> "LBRACE"
  | Raw_parser.LBRACELESS             -> "LBRACELESS"
  | Raw_parser.LBRACKET               -> "LBRACKET"
  | Raw_parser.LBRACKETAT             -> "LBRACKETAT"
  | Raw_parser.LBRACKETATAT           -> "LBRACKETAT"
  | Raw_parser.LBRACKETATATAT         -> "LBRACKETAT"
  | Raw_parser.LBRACKETBAR            -> "LBRACKETBA"
  | Raw_parser.LBRACKETGREATER        -> "LBRACKETGR"
  | Raw_parser.LBRACKETLESS           -> "LBRACKETLE"
  | Raw_parser.LBRACKETPERCENT        -> "LBRACKETPE"
  | Raw_parser.LBRACKETPERCENTPERCENT -> "LBRACKETPE"
  | Raw_parser.LESS                   -> "LESS"
  | Raw_parser.LESSMINUS              -> "LESSMINUS"
  | Raw_parser.LET                    -> "LET"
  | Raw_parser.LETOP x                -> s "LETOP" x
  | Raw_parser.LIDENT x               -> s "LIDENT" x
  | Raw_parser.LPAREN                 -> "LPAREN"
  | Raw_parser.MATCH                  -> "MATCH"
  | Raw_parser.METHOD                 -> "METHOD"
  | Raw_parser.MINUS                  -> "MINUS"
  | Raw_parser.MINUSDOT               -> "MINUSDOT"
  | Raw_parser.MINUSGREATER           -> "MINUSGREAT"
  | Raw_parser.MODULE                 -> "MODULE"
  | Raw_parser.MUTABLE                -> "MUTABLE"
  | Raw_parser.NEW                    -> "NEW"
  | Raw_parser.NONREC                 -> "NONREC"
  | Raw_parser.OBJECT                 -> "OBJECT"
  | Raw_parser.OF                     -> "OF"
  | Raw_parser.OPEN                   -> "OPEN"
  | Raw_parser.OPTLABEL x             -> s "OPTLABEL" x
  | Raw_parser.OR                     -> "OR"
  | Raw_parser.PERCENT                -> "PERCENT"
  | Raw_parser.PLUS                   -> "PLUS"
  | Raw_parser.PLUSDOT                -> "PLUSDOT"
  | Raw_parser.PLUSEQ                 -> "PLUSEQ"
  | Raw_parser.PREFIXOP x             -> s "PREFIXOP" x
  | Raw_parser.PRIVATE                -> "PRIVATE"
  | Raw_parser.QUESTION               -> "QUESTION"
  | Raw_parser.QUOTE                  -> "QUOTE"
  | Raw_parser.RBRACE                 -> "RBRACE"
  | Raw_parser.RBRACKET               -> "RBRACKET"
  | Raw_parser.REC                    -> "REC"
  | Raw_parser.RPAREN                 -> "RPAREN"
  | Raw_parser.SEMI                   -> "SEMI"
  | Raw_parser.SEMISEMI               -> "SEMISEMI"
  | Raw_parser.SIG                    -> "SIG"
  | Raw_parser.STAR                   -> "STAR"
  | Raw_parser.STRING (x, _)          -> s "STRING" x
  | Raw_parser.STRUCT                 -> "STRUCT"
  | Raw_parser.THEN                   -> "THEN"
  | Raw_parser.TILDE                  -> "TILDE"
  | Raw_parser.TO                     -> "TO"
  | Raw_parser.TRUE                   -> "TRUE"
  | Raw_parser.TRY                    -> "TRY"
  | Raw_parser.TYPE                   -> "TYPE"
  | Raw_parser.UIDENT x               -> s "UIDENT" x
  | Raw_parser.UNDERSCORE             -> "UNDERSCORE"
  | Raw_parser.VAL                    -> "VAL"
  | Raw_parser.VIRTUAL                -> "VIRTUAL"
  | Raw_parser.WHEN                   -> "WHEN"
  | Raw_parser.WHILE                  -> "WHILE"
  | Raw_parser.WITH                   -> "WITH"

let dump_tokens tokens =
  List.iter (fun (token, {Lexing. pos_cnum; pos_bol; pos_lnum; _}, _endp) ->
      Printf.printf "% 4d:%02d   %s\n"
        pos_lnum (pos_cnum - pos_bol) (string_of_token token)
    ) tokens

(* Parsing *)

module P = Raw_parser
module I = P.MenhirInterpreter
module Printer = Merlin_recovery.DummyPrinter (I)
(* module Printer = Merlin_recovery.MakePrinter ( *)
(*    struct *)
(*      module I = I *)
(*      let print = Printf.printf "%s" *)
(*      let print_symbol = function *)
(*        | I.X s -> Printf.printf "%s" @@ Parser_recover.print_symbol s *)
(*      let print_element = None *)
(*      let print_token t = print @@ string_of_token t *)
(*    end) *)
 
module R =
  Merlin_recovery.Make(I)
    (struct
      include Parser_recover

      let default_value loc x =
        Default.default_loc := loc;
        default_value x

      let guide _ = false
      
      let use_indentation_heuristic = false
     end)
    (Merlin_recovery.DummyPrinter (I))

type 'a positioned = 'a * Lexing.position * Lexing.position

module type PARSE_INTF = sig
  type 'a parser

  val initial :
    (Lexing.position -> 'a I.checkpoint) ->
    Lexing.position -> 'a parser

  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a
    | Error

  val step : 'a parser -> P.token positioned -> 'a step
end

module type RECOVER_INTF = sig
  include PARSE_INTF
  (* Interface for recovery *)
  val recover : 'a I.checkpoint -> 'a parser
  val recovery_env : 'a parser -> 'a I.env
end

module Without_recovery : RECOVER_INTF =
struct
  type 'a parser = 'a I.checkpoint

  let initial entrypoint position = entrypoint position

  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a
    | Error

  let rec normalize = function
    | I.InputNeeded _ as cp -> Intermediate cp
    | I.Accepted x -> Success x
    | I.HandlingError _ | I.Rejected -> Error
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      normalize (I.resume cp)

  let step cp token =
    normalize (I.offer cp token)

  let recover = function
    | I.InputNeeded _ as cp -> cp
    | _ -> assert false

  let recovery_env = function
    | I.InputNeeded env -> env
    | _ -> assert false
end

module With_recovery : PARSE_INTF =
struct
  module M = Without_recovery

  type 'a parser =
    | Correct of 'a M.parser
    | Recovering of 'a R.candidates

  let initial entry_point position =
    Correct (M.initial entry_point position)

  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a
    | Error

  let step parser token =
    let try_recovery candidates =
      begin match token with (token, _, _) ->
        Printf.printf "Recovery attempt on %s:\n"
            (string_of_token token) end;
      match R.attempt candidates token with
      | `Ok (cp, _) -> Intermediate (Correct (M.recover cp))
      | `Accept x -> Success x
      | `Fail ->
         begin match token with
         | Raw_parser.EOF, _, _ ->
            begin match candidates.final with
            | None -> Error
            | Some x -> Success x
            end
         (* Okay, lets try with next token *)
         | _ -> Intermediate (Recovering candidates)
         end
    in
    match parser with
    | Correct parser ->
      begin match M.step parser token with
        | M.Intermediate parser -> Intermediate (Correct parser)
        | M.Success x -> Success x
        | M.Error ->
          let env = M.recovery_env parser in
          let candidates = R.generate env in
          try_recovery candidates
      end
    | Recovering candidates -> try_recovery candidates
end

let parse_without_recovery entrypoint tokens =
  let module P = Without_recovery in
  let rec step tokens = function
    | P.Error -> failwith "Parsing failed"
    | P.Success x -> x
    | P.Intermediate p -> offer p tokens
  and offer p = function
    | [] -> assert false
    | token :: rest -> step rest (P.step p token)
  in
  offer (P.initial entrypoint Lexing.dummy_pos) tokens


let parse_with_recovery entrypoint tokens =
  let module P = With_recovery in
  let rec step tokens = function
    | P.Error -> failwith "Parsing failed"
    | P.Success x -> x
    | P.Intermediate p -> offer p tokens
  and offer p tokens =
    let token, rest = match tokens with
      | [] -> (Raw_parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos), []
      | token :: rest -> token, rest
    in
    step rest (P.step p token)
  in
  offer (P.initial entrypoint Lexing.dummy_pos) tokens

(* Entrypoint *)

let () =
  let len = Array.length Sys.argv in
  if len = 1 then
    Printf.eprintf
      "Usage: '%s' { '-debug' | 'filename.ml' | 'filename.mli' }"
      Sys.argv.(0)
  else
    let debug = ref false in
    for i = 1 to len - 1 do
      match Sys.argv.(i) with
      | "-debug" -> debug := true
      | fname ->
        let tokens = lex_file fname in
        (* dump_tokens tokens; *)
        (* Interface or implementation? Check last character *)
        (*let parse = parse_without_recovery in*)
        let parse = parse_with_recovery in
        if fname <> "" && fname.[String.length fname - 1] = 'i' then
          let intf = parse P.Incremental.interface tokens in
          Format.printf "%a\n%!" Pprintast.signature intf
        else
          let impl = parse P.Incremental.implementation tokens in
          Format.printf "%a\n%!" Pprintast.structure impl
    done

