open Parsing;;
open Lexing;;
open List;;
open String;;

open Lambda;;
open Parser;;
open Lexer;;

let read_line_semicolon() =
  let rec read_aux linea = 
    let linea2 = read_line() in
      if (contains linea2 ';') then 
        let pos = (rindex_from linea2 ((length linea2) - 1) ';') in
          if (pos = 0) then
            read_aux (linea)
          else
            if (linea2.[(pos - 1)] = ';') then (linea ^ (sub linea2 0 (pos - 1)))
            else read_aux (linea ^ (sub linea2 0 (pos - 1)) ^ " ")
      else read_aux (linea ^ linea2 ^ " ")
  in read_aux ""
;;

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

