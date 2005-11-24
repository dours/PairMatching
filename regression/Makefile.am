# List all tests to run
TESTS = test001

# Set testing script with optional environment valiables
TESTS_ENVIRONMENT = ./randomTest.sh

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

# extra compiled native libraries used for native linking
EXTRA_CMXA := $(EXTRA_CMA:.cma=.cmxa)

# OCaml parser call with flags
#AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
#AM_P4FLAGS = -pp "$(CAMLP4R) $(PARSER_INCLUDES)"

# bytecode compiling flags
AM_BFLAGS = -w p $(MODULE_INCLUDES) $(DEBUG)
# native (optimized) compiling flags
AM_OFLAGS = -w p $(MODULE_INCLUDES) $(DEBUG)

# Clean all generated output
clean-am:
	rm -f *.cmi *.cmx *.o
	rm -fR testingResult*
	rm -f $(addsuffix $(EXEEXT), $(TESTS))

test001: test001.ml
	$(OCAMLOPT) $(BFLAGS) $(P4FLAGS) -o $@$(EXEEXT) $(EXTRA_CMXA) $(PACKAGE).cmxa $<
