AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libcheck_cnt.la 

noinst_HEADERS = \
  check_cntLinkDef.h 

include_HEADERS = \
  check_cnt.h

libcheck_cnt_la_SOURCES = \
  check_cnt.C \
  check_cnt_Dict.C

libcheck_cnt_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco

# Rule for generating CINT dictionaries from class headers.
check_cnt_Dict.C: \
  check_cnt.h \
  check_cntLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

