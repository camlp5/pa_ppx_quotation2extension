WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_quotation2extension,ounit2,pa_ppx.utils

ML= \
	pa_ppx_quotation2extension_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_quotation2extension_test.exe

all: $(TESTS)

test: all
	mkdir -p _build && ./pa_ppx_quotation2extension_test.exe

pa_ppx_quotation2extension_test.exe: pa_ppx_quotation2extension_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

