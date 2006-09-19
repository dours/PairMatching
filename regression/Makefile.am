TESTS = test001

TESTS_ENVIRONMENT = $(srcdir)/randomTest.sh

include $(top_srcdir)/m4/Makefile.am.vars

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P4FLAGS =

TARGET = 

CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log testingResult* *.cmi *.cmx *.o

include $(top_srcdir)/m4/Makefile.am.targets

# Rules to build tests
%: %.ml
	$(OCAMLOPT) $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $(EXTRA_CMXA) $<

include $(top_srcdir)/m4/Makefile.am.rules
