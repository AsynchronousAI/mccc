#
# Special variables should not be exported
#
unexport LIBDIRS
unexport EXTLIBS
unexport CMIS
unexport CMOS
unexport TARGET

ifdef NATIVECAML
  CAMLC         = ocamlopt
  CAMLLINK      = ocamlopt
  CAMLLIB       = $(shell ocamlopt -where)
  CMO           = cmx
  CMXA          = cmxa
  EXEEXT        = .asm
else
  CAMLC         = ocamlc
  CAMLLINK      = ocamlc
  CAMLLIB       = $(shell ocamlc -where)
  CMO           = cmo
  CMXA          = cma
  EXEEXT        = .byte
endif

%.$(CMO): %.ml
	$(CAMLC) $(LIBDIRS) -c $<

%.cmi: %.mli
	$(CAMLC) $(LIBDIRS) -c $<

.PHONY: default
default: $(TARGET)

$(TARGET): $(CMIS) $(CMOS) $(EXTLIBS)
	$(CAMLC) -a $(CMOS) -o $(TARGET)

.PHONY: clean
clean: 
	-rm -if *.o *.a *.cmi *.cmo *.cmx *.cma *.cmxa $(TARGET)

