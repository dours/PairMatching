TESTS = test001

TESTS_ENVIRONMENT = $(srcdir)/randomTest.sh

include $(top_srcdir)/m4/Makefile.am.vars

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P5FLAGS =

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

TARGET = 

CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log testingResult* *.cmi *.cmx *.o

include $(top_srcdir)/m4/Makefile.am.targets

# Rules to build tests
%: %.ml
	$(OCAMLOPT) $(BFLAGS) $(PXFLAGS) -o $@$(EXEEXT) $(EXTRA_CMXA) $<

include $(top_srcdir)/m4/Makefile.am.rules
