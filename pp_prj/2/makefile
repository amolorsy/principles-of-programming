prj2_actual : commonGrade.cmo prj2.cmo prj2_actualgrade.ml
	ocaml commonGrade.cmo prj2.cmo prj2_actualgrade.ml

prj2 : commonGrade.cmo prj2.cmo prj2_grade.ml
	ocaml commonGrade.cmo prj2.cmo prj2_grade.ml

commonGrade.cmo : commonGrade.ml
	ocamlc commonGrade.ml

prj2.cmo : prj2.ml
	ocamlc prj2.ml

clean :
	rm -f *.cmo *.cmi
