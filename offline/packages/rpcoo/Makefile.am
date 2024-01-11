AUTOMAKE_OPTIONS = foreign

SUBDIRS = base interface modules

INCLUDES =

# Rule for generating CINT dictionaries.

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
