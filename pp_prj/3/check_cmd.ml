let filename = 
  if Array.length Sys.argv < 2
  then (prerr_endline "Usage: ./check_cmd filename.cmd";
        exit (-1))
  else Sys.argv.(1)

let file_channel = open_in filename
let lexbuf = Lexing.from_channel file_channel
let c = Cmd_parser.main Cmd_lexer.token lexbuf
let _ = close_in file_channel

let _ = print_endline "========== cmd print =========="
let _ = T.pprint_cmd c

let _ = print_endline "========== check_cmd result =========="
let check_cmd_result = T.check_cmd c
let _ = print_endline (string_of_bool check_cmd_result)
