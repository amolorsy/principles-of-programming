let filename = 
  if Array.length Sys.argv < 2
  then (prerr_endline "Usage: ./main filename.exp";
        exit (-1))
  else Sys.argv.(1)

let file_channel = open_in filename
let lexbuf = Lexing.from_channel file_channel
let e = Exp_parser.main Exp_lexer.token lexbuf
let _ = close_in file_channel

let _ = print_endline "========== exp print =========="
let _ = T.pprint_exp e

let _ = print_endline "========== check_exp result =========="
let check_exp_result = T.check_exp e
let _ = print_endline (string_of_bool check_exp_result)
let _ = if check_exp_result then () else exit 0

let _ = print_endline "========== transform result =========="
let c = T.transform e
let _ = T.pprint_cmd c

let _ = print_endline "========== check_cmd result =========="
let check_cmd_result = T.check_cmd c
let _ = print_endline (string_of_bool check_cmd_result)
let _ = if check_cmd_result then () else exit 0

let _ = print_endline "========== transform succeeded =========="
