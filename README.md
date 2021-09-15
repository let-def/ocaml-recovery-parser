<!--
Copyright (c) 2019 Frédéric Bour

SPDX-License-Identifier: MIT
-->

# OCaml Recovery Parser
A simple fork of OCaml parser with support for error recovery

## Recovery generator

The recovery generator is implemented in `menhir-recover`.
It is a binary that consumes a `cmly` file and produces an OCaml source module that contains recovery heuristics suitable to use by the `Merlin_recovery` module.

`cmly` file are produced by menhir when you pass the `--cmly` flag. It is a marshalled representation of the grammar and the automaton suitable for post-processing a grammar. It is a bit like a `ppx` that post-processes an OCaml AST, although here no rewriting happens. Rather, information can be programmatically extracted from a parser.

## Integration to dune

The recovery runtime engine works with the `--table` engine of Menhir with  `--inspection` extension. The recovery generator needs to postprocess the `cmly` file.

Therefore, the `menhir` rule in dune should be given the right flags:

```scheme
 (menhir
  (flags --inspection --table --cmly)
  (modules parser))
```

A table-based parser requires the `menhirLib` library, so add `(libraries menhirLib)` to your dune target.

After that, we need to derive a `recover.ml` module associated to the parser:

```scheme
(rule
 (targets recover.ml)
 (deps parser.cmly)
 (action
  (with-stdout-to
   %{targets}
   (run ../menhir-recover/main.exe parser.cmly))))
```

That's all on the build system side.

## Before recovery: take control back!

By default, Menhir provides a simple `ocamlyacc`-style parsing function for each entry-point:

```ocaml
val entry_point : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> sem_value
```

It takes a lexing function, a lexing buffer, and returns a semantic value (generally an AST) if successful, or raises an exception.

This is sufficient for simple parsing tasks but quite inflexible:

- either to whole task succeeds, and you get access to the AST, or it fails and all intermediate computations are lost
- parsing in synchronous: you cannot suspend parsing while waiting for input, parse inside Lwt, etc.
- ...

In other word, Menhir grabs control. To lift these limitations it also provides an incremental interface that gives back control to the programmer.

The `checkpoint` type represents the state of the parser. When you have a token, you can `offer` it to the parser and you will get a new checkpoint that represents success, failure or another intermediate parser state that needs more tokens. A `checkpoint` is pure (up to user-procided semantic actions), so if you keep track of previous checkpoints you can backtrack and explore different parsing futures.

A simple parsing loop can be implemented like that:

```ocaml
module I = Parser.MenhirInterpreter

type 'a positioned = 'a * Lexing.position * Lexing.position
let rec loop tokens cp =
  match cp with
  | I.Accepted x -> Some x
  | I.Rejected -> None
  | I.Shifting (_, _, _) | I.AboutToReduce (_, _) | I.HandlingError _ ->
    loop tokens (I.resume cp)
  | I.InputNeeded _ ->
    match tokens with
    | [] -> assert false
    | token :: tokens -> loop tokens (I.offer cp token)

val loop : Parser.token positioned list -> 'a I.checkpoint -> 'a option
           
```

It takes an explicit list of tokens, offer all of them to the parser, and returns `Some ast` if parsing succeeds. This is a minimal introduction to the incremental interface of Menhir and does not enable us to do anything new yet.

For each entry point, Menhir generates a function that will give you an initial checkpoint:

```ocaml
val Parser.Incremental.entry_point :
  Lexing.position -> sem_value I.checkpoint
```

## Astracting parsing interface

To give a similar interface to the normal and recoverable parser, and to build the recovery on top of normal parsing, we will use the following abstraction:

```ocaml
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
```

This just gives a bit more structure to the code from previous section, and consumes token one at a time. When input is wrong, we just get `Error`. Instead we would like get access to the internal state of the parser to see what went wrong and what can be done about it.

So we will extend the interface with two functions to break the abstraction: one to get access to the checkpoint, and one to produce a new parser from a custom checkpoint. 

```ocaml
module type RECOVER_INTF = sig
  include PARSE_INTF
  val recover : 'a I.checkpoint -> 'a parser
  val recovery_env : 'a parser -> 'a I.env
end
```

And here is an implementation of normal parsing that satisfy this interface: 

```OCaml
module Parser : RECOVER_INTF =
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
end
```

## Using the recoverable parser

On top of the previous abstraction we will build the recoverable parser.

First, we will make use of the meta-data produced by `menhir-recover` executable (it uses the `Recover` module produced by the dune rule above):

```ocaml
module R =                         
  Merlin_recovery.Make(I)          
    (struct                        
       include Recover       
                                   
       let default_value loc x =    
         Default.default_loc := loc;
         default_value x            
                                   
       let guide _ = false          
    end)
```

`Merlin_recovery` implements indentation guided recovery heuristic on top of the meta-data.

The two `default_value` and `guide` extra functions lets you customize the recovery. You can ignore them for now, we just provided reasonable defaults here.

And now the robust parser:

```ocaml
module RP : PARSE_INTF =
struct
  type 'a parser =
    | Correct of 'a P.parser
    | Recovering of 'a R.candidates

  let initial entry_point position =
    Correct (P.initial entry_point position)

  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a
    | Error

  let step parser token =
    match parser with
    | Correct parser ->
      begin match P.step parser token with
        | P.Intermediate parser -> Intermediate (Correct parser)
        | P.Success x -> Success x
        | P.Error ->
          let env = P.recovery_env parser in
          Intermediate (Recovering (R.generate env))
      end
    | Recovering candidates ->
      begin match R.attempt candidates token with
        | `Ok (cp, _) -> Intermediate (Correct (P.recover cp))
        | `Accept x -> Success x
        | `Fail ->
          begin match token with
            | Raw_parser.EOF, _, _ ->
              begin match candidates.final with
                | None -> Error
                | Some x -> Success x
              end
            | _ -> Intermediate parser
          end
      end
end
```

It keeps using the normal parser as long as it works, and switch to the recovery heuristic on failure, hiding the fact that the normal parser returned an `Error`. If recovery fails, it propagates the `Error`, otherwise it returns to normal parsing.

## Making the parser recovery friendly`

A recovery friendly parser should not raise exceptions and should provide recovery annotations for the a few critical grammatical constructions.

### Exceptions and `error

The recovery algorithm take over the normal parser to continue parsing. An error situation occurs when there are no transition on the next token.

However recovery is blind to exceptions that are raised by semantic actions. Failing actions could lead the recovery to a dead-end and make it fail just like the normal parser. Strictly speaking, it is possible to instruct the recovery to avoid raising semantic actions, however the rule of thumb is to **avoid exceptions in semantic actions**.

Although less important, it is good practice to avoid using the `error` token. The `error` mechanism of Menhir becomes a bit redundant in presence of parser instrumentation and there are better ways to achieve similar results  (ask me if necessary :)).

### Recovery annotation

To recover, the algorithm will synthesize artificial symbols (`terminals`/`tokens` and `non-terminals`) to complete the current parsing context (for instance, provide a `)` to finish a mis-bracketed expression, or an identifier after an unterminated `foo.`).

Non-parameterized symbols are easy to synthesized (e.g. `)` require no extra input), but parameterized terminals and non-terminals require more information to be produced.

#### Annotated terminals

After `%token<t> FOO` declaration, it is possible to add an annotation `[@recover.expr ocaml_expression]` where `ocaml_expression` has type `t`.

The `FOO` token will now be considered by the recovery algorithm when it tries to find a way to complete an unterminated sentence. The expression will be evaluated every time the recovery algorithm needs to produce a `FOO` token.

For instance, in the annotated OCaml parser, it is sometime necessary to produce identifiers:

```ocaml
%token <string> LIDENT
  [@recover.expr "<invalid-lident>"]
```

#### Annotated non-terminals

`recover.expr` annotations can also be put on non-terminals.

For instance, when an expression is missing, it is better to provide a placeholder expression than to let the recovery synthesize an arbitrary expression (even though `()` is an harmless one that is likely to be generated).

To this end, the following has been added to the prelude of the OCaml parser:

```ocaml
%[@recover.prelude

  open Parsetree
  open Ast_helper
  
  ...

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))
  
  ...
]
```

and finally in the main grammar:

```ocaml
expr [@recover.expr default_expr ()]:
...
```

The prelude is a set of definitions shared by all recovery expressions. Now, `default_expr ()` will be evaluated whenever an expression is needed.

#### Insufficient annotations

If the grammar does not provide enough annotations, recovery will not always be possible.

For instance if we remove the `recover.expr` annotation on `LIDENT` in the grammar, the post-processor will output a huge error log that look like:

```
Not enough annotation to recover from state 116:
optlabel ::= QUESTION . LIDENT COLON

Not enough annotation to recover from state 127:
signature_item                                           ::= TYPE . (ext = ext) list(attribute) NONREC                     (params = type_parameters) type_longident                   PLUSEQ                                                (priv = private_flag)                                        (xs = reversed_bar_llist(extension_constructor_declaration)) list(post_item_attribute)
                                                           | TYPE . (ext = ext) list(attribute) (params = type_parameters) type_longident             PLUSEQ                           (priv = private_flag)                                 (xs = reversed_bar_llist(extension_constructor_declaration)) list(post_item_attribute)                                   
generic_type_declaration(nonrec_flag,type_kind)          ::= TYPE . (ext = ext) list(attribute) NONREC                     (params = type_parameters) LIDENT                           (kind_priv_manifest = type_kind)                      (xs = reversed_llist(preceded(CONSTRAINT,constrain)))        list(post_item_attribute)                                   
...
```

If we look at all the states that are stuck, we see that they always involve an identifier. In our case, it is quite easy to see that we should add annotations on `LIDENT` , but in general you should look first for parameterized terminals to annotate.