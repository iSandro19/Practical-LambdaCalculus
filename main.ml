open Parsing;;
open Lexing;;
open List;;
open String;;

open Lambda;;
open Parser;;
open Lexer;;

let read_line_semicolon () =
  let rec aux command =
    let line = read_line () in
      match (index_opt line ';') with
          Some pos -> if (length line) != 1 then
                        (command ^ " " ^ (sub line 0 pos))
                      else
                        command 
        | None -> aux (command ^ " " ^ line)
  in aux "";;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (read_line_semicolon ())) in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)

  ;;

top_level_loop ()
;;

