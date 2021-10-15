(*
 * SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
 * SPDX-License-Identifier: MPL-2.0
 *)

open MenhirSdk.Cmly_api

module Make (G : GRAMMAR):
sig
  val print : string -> unit
end = struct
  open G
  open Format



  let print_table ?(header = "") ?(options = "") table ppf =
    fprintf ppf "<table>\n<th>%s</th>\n" header;
    List.iter (fun row -> 
      fprintf ppf "<tr>";
      List.iter (fun cell ->
        fprintf ppf "<td%s>" options;
        cell ppf;
        fprintf ppf "</td\n>";
      ) row;
      fprintf ppf "</tr>\n";
    ) table;
    fprintf ppf "</table>\n"
  
  let print_LR0 lr =
    let table = [
      [Fun.flip fprintf "Incoming"];
      [(match Lr0.incoming (Lr1.lr0 lr) with
        | None -> Fun.flip fprintf "None"
        | Some symbol -> Fun.flip Print.symbol symbol)];
      [Fun.flip fprintf "Items"]
      ] @ 
      List.map (fun item ->
        [Fun.flip Print.item item]
      ) @@ Lr0.items @@ Lr1.lr0 lr in
    print_table table ~header:"LR0"

  let print_transitions lr =
    let table = 
      List.map (fun (symbol, st) ->
        [
        (fun ppf -> fprintf ppf "%d" @@ Lr1.to_int st);
        (fun ppf -> 
          fprintf ppf "<a href=\"%d.html\">" @@ Lr1.to_int st;
          Print.symbol ppf symbol;
          fprintf ppf "</a>")
        ]
      ) @@ Lr1.transitions lr in
    print_table table ~header:"TRANSITIONS"

  let print_reductions lr = 
    let table = 
      List.map (fun (terminal, prods) ->
        [Fun.flip Print.terminal terminal]
        @ 
        List.map (fun pr ->
          (Fun.flip Print.production pr);
        ) prods;
      ) @@ Lr1.reductions lr in
    print_table table ~header:"REDUCTIONS"
  
  let print graph_folder =
    Lr1.iter (fun lr ->
      let file_name = graph_folder ^ string_of_int (Lr1.to_int lr) ^ ".html" in
      let channel = open_out file_name in
      let ppf = formatter_of_out_channel channel in
      let body = [[
        print_LR0 lr;
        print_transitions lr;
        print_reductions lr
        ]] in
      fprintf ppf "<!DOCTYPE html>\n<html>\n<body>\n";
      fprintf ppf "<h1>%d</h1>\n" @@ Lr1.to_int lr;
      print_table body ppf ~options:" style=\"vertical-align:top\"";
      fprintf ppf "</body>\n</html>\n";
      pp_print_flush ppf ();
      close_out channel)
end
