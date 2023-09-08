OCAMLMAKEFILE = ../OCamlMakefile

RESULT = sme
SOURCES = \
  label.ml \
  reactive.ml \
  io.ml \
  domain.ml \
  browser.ml \
  policyfun.ml \
  policy.ml \
  wrapper.ml \
  release.ml \
  test.ml 
  
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

# all: test.cmo util.cmo 

reactive.cmo: reactive.ml
	ocamlc -c reactive.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

label.cmo: label.ml domain.cmo
	ocamlc -c label.ml

io.cmo: io.ml domain.cmo label.cmo
	ocamlc -c io.ml

release.cmo: release.ml io.cmo label.cmo
	ocamlc -c release.ml

browser.cmo: browser.ml release.cmo reactive.cmo io.cmo domain.cmo
	ocamlc -c browser.ml

test.cmo: test.ml browser.cmo policyfun.cmo wrapper.cmo domain.cmo io.cmo reactive.cmo label.cmo
	ocamlc -c test.ml 

util.cmo: util.ml
	ocamlc -c util.ml

policyfun.cmo: policyfun.ml domain.cmo io.cmo label.cmo
	ocamlc -c policyfun.ml 

policy.cmo: policy.ml policyfun.ml io.ml label.cmo
	ocamlc -c policy.ml

wrapper.cmo: wrapper.ml release.cmo reactive.cmo browser.cmo policyfun.cmo policy.cmo label.cmo
	ocamlc -c wrapper.ml 

# clean:
# 	rm -rf main *.cmo *.cmi doc latex-doc main.tex ocamldoc.sty ocamldoc.out *.aux *.log *.dvi

# .PHONY: all clean


