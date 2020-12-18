type token = Raw_parser.token =
  | WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of (string)
  | TYPE
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * string option)
  | STAR
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of (string)
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OR
  | OPTLABEL of (string)
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH
  | LPAREN
  | LIDENT of (string)
  | LETOP of (string)
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of (string)
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of (string)
  | INFIXOP3 of (string)
  | INFIXOP2 of (string)
  | INFIXOP1 of (string)
  | INFIXOP0 of (string)
  | INCLUDE
  | IN
  | IF
  | HASHOP of (string)
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATER
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (string * char option)
  | FALSE
  | EXTERNAL
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | DOWNTO
  | DOTOP of (string)
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of (Docstrings.docstring)
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of (char)
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | ANDOP of (string)
  | AND
  | AMPERSAND
  | AMPERAMPER

type source = (token * Lexing.position * Lexing.position) array
type token_index = int

type bracket_family =
  | Parenthesis
  | Bracket
  | BracketBar
  | Brace
  | BraceLess
  | If_Then
  | For_While
  | Begin_Object_Struct_Sig

let same_family a b = match a, b with
  | Parenthesis             , Parenthesis
  | Bracket                 , Bracket
  | BracketBar              , BracketBar
  | Brace                   , Brace
  | BraceLess               , BraceLess
  | If_Then                 , If_Then
  | For_While               , For_While
  | Begin_Object_Struct_Sig , Begin_Object_Struct_Sig -> true
  | ( Parenthesis | Bracket | BracketBar | Brace | BraceLess
    | If_Then | For_While | Begin_Object_Struct_Sig ), _ -> false

type bracket_kind =
  | Opening
  | Closing

type bracket = bracket_family * bracket_kind

let classify = function
  | BEGIN                  -> Some (Begin_Object_Struct_Sig, Opening)
  | OBJECT                 -> Some (Begin_Object_Struct_Sig, Opening)
  | SIG                    -> Some (Begin_Object_Struct_Sig, Opening)
  | STRUCT                 -> Some (Begin_Object_Struct_Sig, Opening)
  | END                    -> Some (Begin_Object_Struct_Sig, Closing)
  | LBRACELESS             -> Some (BraceLess, Opening)
  | GREATERRBRACE          -> Some (BraceLess, Closing)
  | IF                     -> Some (If_Then, Opening)
  | THEN                   -> Some (If_Then, Closing)
  | LBRACE                 -> Some (Brace, Opening)
  | RBRACE                 -> Some (Brace, Closing)
  | LBRACKETATATAT         -> Some (Bracket, Opening)
  | LBRACKETATAT           -> Some (Bracket, Opening)
  | LBRACKETAT             -> Some (Bracket, Opening)
  | LBRACKETGREATER        -> Some (Bracket, Opening)
  | LBRACKETLESS           -> Some (Bracket, Opening)
  | LBRACKETPERCENTPERCENT -> Some (Bracket, Opening)
  | LBRACKETPERCENT        -> Some (Bracket, Opening)
  | LBRACKET               -> Some (Bracket, Opening)
  | RBRACKET               -> Some (Bracket, Closing)
  | GREATERRBRACKET        -> Some (Bracket, Closing)
  | LBRACKETBAR            -> Some (BracketBar, Opening)
  | BARRBRACKET            -> Some (BracketBar, Closing)
  | LPAREN                 -> Some (Parenthesis, Opening)
  | RPAREN                 -> Some (Parenthesis, Closing)
  | FOR                    -> Some (For_While, Opening)
  | WHILE                  -> Some (For_While, Opening)
  | DONE                   -> Some (For_While, Closing)
  | _                      -> None


let rec skip_attr_id source i =
  if i = Array.length source then i else
    let token, _, _ = source.(i) in
    match token with
    | COMMENT _ | DOCSTRING _ ->
      skip_attr_id source (i + 1)
    | LIDENT _ | UIDENT _ | AND | AS | ASSERT | BEGIN | CLASS | CONSTRAINT | DO
    | DONE | DOWNTO | ELSE | END | EXCEPTION | EXTERNAL | FALSE | FOR | FUN
    | FUNCTION | FUNCTOR | IF | IN | INCLUDE | INHERIT | INITIALIZER | LAZY
    | LET | MATCH | METHOD | MODULE | MUTABLE | NEW | NONREC | OBJECT | OF
    | OPEN | OR | PRIVATE | REC | SIG | STRUCT | THEN | TO | TRUE | TRY | TYPE
    | VAL | VIRTUAL | WHEN | WHILE | WITH ->
      skip_dot source (i + 1)
    | _ -> i

and skip_dot source i =
  if i = Array.length source then i else
    let token, _, _ = source.(i) in
    match token with
    | COMMENT _ | DOCSTRING _ ->
      skip_attr_id source (i + 1)
    | DOT ->  skip_attr_id source (i + 1)
    | _ -> i

let rec parse source acc i =
  if i = Array.length source
  then List.rev acc
  else
    let token, _, _ = source.(i) in
    let i' = match token with
      | LBRACKETPERCENT | LBRACKETPERCENTPERCENT
      | LBRACKETAT | LBRACKETATAT | LBRACKETATATAT ->
        skip_attr_id source (i + 1)
      | _ -> i + 1
    in
    let acc = match classify token with
      | Some role -> ((i, role) :: acc)
      | None -> acc
    in
    parse source acc i'

let parse (source: source): (token_index * bracket) list = parse source [] 0

let print_bracket = function
  | (Begin_Object_Struct_Sig, Opening) -> "Begin"
  | (Begin_Object_Struct_Sig, Closing) -> "End"
  | (Brace, Opening) -> "{"
  | (Brace, Closing) -> "}"
  | (BraceLess, Opening) -> "{<"
  | (BraceLess, Closing) -> ">}"
  | (BracketBar, Opening) -> "[|"
  | (BracketBar, Closing) -> "|]"
  | (Bracket, Opening) -> "["
  | (Bracket, Closing) -> "]"
  | (For_While, Opening) -> "For/While"
  | (For_While, Closing) -> "Done"
  | (If_Then, Opening) -> "If"
  | (If_Then, Closing) -> "Then"
  | (Parenthesis, Opening) -> "("
  | (Parenthesis, Closing) -> ")"

type source_bracket = token_index * bracket

let print oc (source: source) (brackets: source_bracket list) : int =
  let process (indent, last_line) (index, bracket) =
    let _, pos, _ = source.(index) in
    let current_line = pos.pos_lnum in
    let on_same_line = current_line = last_line in
    if on_same_line then output_char oc ' '
    else begin
      Printf.fprintf oc "\n%04d" current_line;
      for _i = 0 to pos.pos_cnum - pos.pos_bol do
        output_char oc ' '
      done
    end;
    let indent, current_indent = match snd bracket with
      | Opening -> indent    , indent + 1
      | Closing -> indent - 1, indent - 1
    in
    output_string oc (print_bracket bracket);
    output_char oc '-';
    output_string oc (string_of_int indent);
    (current_indent, current_line)
  in
  let _, indent = List.fold_left process (0, 0) brackets in
  output_char oc '\n';
  indent

type bracket_tree =
  Bracket of source_bracket * bracket_forest * source_bracket

and bracket_forest =
  bracket_tree list

let group_correct
    (brackets : source_bracket list)
  : (bracket_tree, source_bracket) result list
  =
  let return_forest forest result =
    List.fold_left
      (fun result tree -> Ok tree :: result)
      result forest
  in
  let rec parse_top acc = function
    | (_, (_, Opening) as bracket) :: tail ->
      let sub_parse, tail = parse_family bracket [] tail in
      let acc = match sub_parse with
        | Ok _ as ok -> ok :: acc
        | Error parts -> List.rev_append parts acc
      in
      parse_top acc tail
    | (_, (_, Closing) as bracket) :: tail ->
      parse_top (Error bracket :: acc) tail
    | [] -> List.rev acc
  and parse_family (_, (family, _) as opening) (forest : bracket_forest) = function
    | (_, (family', Closing) as closing) :: tail ->
      if same_family family family'
      then Ok (Bracket (opening, List.rev forest, closing)), tail
      else Error (Error opening :: return_forest forest [Error closing]), tail
    | (_, (_, Opening) as opening') :: tail ->
      begin match parse_family opening' [] tail with
        | Ok tree, tail -> parse_family opening (tree :: forest) tail
        | Error tokens, tail -> Error (Error opening :: tokens), tail
      end
    | [] -> Error (Error opening :: return_forest forest []), []
  in
  parse_top [] brackets

let print_incorrect oc source
    (groups : (bracket_tree, source_bracket) result list) =
  let groups =
    List.filter_map (function Ok _ -> None | Error e -> Some e)
      groups
  in
  print oc source groups
