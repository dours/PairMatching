# List all tests to run
TESTS = test001

# Set testing script with optional environment valiables
TESTS_ENVIRONMENT = ./randomTest.sh

# Clean all generated output
clean-am:
	rm -f $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
	rm -f $(addsuffix .cmx, $(TESTS)) $(addsuffix .o, $(TESTS))
	rm -f *.diff *.log test001

test001: test001.ml
	$(OCAMLOPT) $(BFLAGS) $(P4FLAGS) -I .. -o $@ $(EXTRA_CMXA) PM.cmxa $<
