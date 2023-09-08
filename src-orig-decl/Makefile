OCAMLMAKEFILE = ../OCamlMakefile

RESULT = sme
SOURCES = \
  level.ml \
  reactive.ml \
  io.ml \
  domain.ml \
  policyfun.ml \
  policy.ml \
  browser.ml \
  wrapper.ml \
  test.ml 
  
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

# all: test.cmo util.cmo 

reactive.cmo: reactive.ml
	ocamlc -c reactive.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

level.cmo: level.ml domain.cmo
	ocamlc -c level.ml

io.cmo: io.ml domain.cmo level.cmo
	ocamlc -c io.ml

browser.cmo: browser.ml reactive.cmo io.cmo domain.cmo
	ocamlc -c browser.ml

test.cmo: test.ml browser.cmo policyfun.cmo wrapper.cmo domain.cmo io.cmo reactive.cmo level.cmo
	ocamlc -c test.ml 

util.cmo: util.ml
	ocamlc -c util.ml

policyfun.cmo: policyfun.ml domain.cmo io.cmo level.cmo
	ocamlc -c policyfun.ml 

policy.cmo: policy.ml policyfun.ml io.ml level.cmo
	ocamlc -c policy.ml

wrapper.cmo: wrapper.ml reactive.cmo browser.cmo policyfun.cmo policy.cmo level.cmo
	ocamlc -c wrapper.ml 

# clean:
# 	rm -rf main *.cmo *.cmi doc latex-doc main.tex ocamldoc.sty ocamldoc.out *.aux *.log *.dvi

# .PHONY: all clean


