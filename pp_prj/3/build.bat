ocamlc -c trans.ml
ocamlc -c t.ml

ocamllex exp_lexer.mll
ocamlyacc exp_parser.mly
ocamlc -c exp_parser.mli
ocamlc -c exp_lexer.ml
ocamlc -c exp_parser.ml

ocamllex cmd_lexer.mll
ocamlyacc cmd_parser.mly
ocamlc -c cmd_parser.mli
ocamlc -c cmd_lexer.ml
ocamlc -c cmd_parser.ml

ocamlc -c check_exp.ml
ocamlc -c check_cmd.ml
ocamlc -c main.ml

ocamlc -o check_exp.exe t.cmo exp_lexer.cmo exp_parser.cmo check_exp.cmo
ocamlc -o check_cmd.exe t.cmo cmd_lexer.cmo cmd_parser.cmo check_cmd.cmo
ocamlc -o main.exe t.cmo exp_lexer.cmo exp_parser.cmo main.cmo