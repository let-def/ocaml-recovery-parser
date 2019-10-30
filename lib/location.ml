type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

let none =
  let pos = { Lexing.dummy_pos with Lexing.pos_fname = "_none_" } in
  { loc_start = pos; loc_end = pos; loc_ghost = false }

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt; loc }
let mknoloc txt = { txt; loc = none }

let curr lexbuf =
  { loc_start = lexbuf.Lexing.lex_start_p;
    loc_end = lexbuf.Lexing.lex_curr_p;
    loc_ghost = false }

let msg ~loc:_ _ = ()

let errorf ~loc ?sub fmt =
  ignore sub;
  let k msg =
    Printf.printf "C'est pas bien ligne %d, colonne %d: %s"
      loc.loc_start.pos_lnum
      (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
      msg
  in
  Format.kasprintf k fmt
