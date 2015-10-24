hw1_test : hw7_1.cmo
	ocaml hw7_1.cmo test.ml
hw1-1 : commonGrade.cmo hw7_1.cmo
	ocaml commonGrade.cmo hw7_1.cmo hw7_1_grade.ml
hw1-2 : commonGrade.cmo hw7_1.cmo
	ocaml commonGrade.cmo hw7_1.cmo hw7_1_grade_2.ml
hw1_actual : commonGrade.cmo hw7_1.cmo
	ocaml commonGrade.cmo hw7_1.cmo hw7_1_actualgrade.ml
hw2 : commonGrade.cmo hw7_2.cmo
	ocaml commonGrade.cmo hw7_2.cmo hw7_2_grade.ml
hw2_actual : commonGrade.cmo hw7_2.cmo
	ocaml commonGrade.cmo hw7_2.cmo hw7_2_actualgrade.ml
hw3-1 : commonGrade.cmo hw7_3.cmo
	ocaml commonGrade.cmo hw7_3.cmo hw7_3_grade.ml
hw3-2 : commonGrade.cmo hw7_3.cmo
	ocaml commonGrade.cmo hw7_3.cmo hw7_3_grade_2.ml
hw3_actual : commonGrade.cmo hw7_3.cmo
	ocaml commonGrade.cmo hw7_3.cmo hw7_3_actualgrade.ml

commonGrade.cmo : commonGrade.ml
	ocamlc commonGrade.ml

hw7_1.cmo : hw7_1.ml
	ocamlc hw7_1.ml
hw7_2.cmo : hw7_2.ml
	ocamlc hw7_2.ml
hw7_3.cmo : hw7_3.ml
	ocamlc hw7_3.ml

clean :
	rm -f *.cmo *.exe *.cmi
