AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libcheck_crk.la 

noinst_HEADERS = \
  check_crkLinkDef.h 

include_HEADERS = \
  check_crk.h

libcheck_crk_la_SOURCES = \
  check_crk.C \
  check_crk_Dict.C

libcheck_crk_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco

# Rule for generating CINT dictionaries from class headers.
check_crk_Dict.C: \
  check_crk.h \
  check_crkLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

