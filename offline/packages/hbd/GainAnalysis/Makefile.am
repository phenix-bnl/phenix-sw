AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(includedir)/.. -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libhbdGainAna.la 

noinst_HEADERS = \
  HbdGainAnalyzerLinkDef.h \
  HbdGainAnalyzer.h

include_HEADERS = 


libhbdGainAna_la_SOURCES = \
  HbdGainAnalyzer.C \
  HbdGainAnalyzer_Dict.C

libhbdGainAna_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lfun4all \
  -lheader \
  -lnanoDST \
  -lphflag \
  -lphool \
  -lphtime \
  -lSubsysReco \
  -lWrappers

# Rule for generating CINT dictionaries from class headers.
HbdGainAnalyzer_Dict.C: \
  HbdGainAnalyzer.h \
  HbdGainAnalyzerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

