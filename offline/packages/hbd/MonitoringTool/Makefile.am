AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libHbdSimpleMonitor.la 

noinst_HEADERS = 

include_HEADERS = \
  HbdSimpleMonitor.h

libHbdSimpleMonitor_la_SOURCES = \
  HbdSimpleMonitor.C \
  HbdSimpleMonitorDict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libHbdSimpleMonitor_la_LIBADD = \
  -lfun4all \
  -lgea \
  -lheader \
  -lnanoDST \
  -lpdbcalBase \
  -lphflag \
  -lphool \
  -lphtime \
  -lPISARoot \
  -lSubsysReco \
  -lWrappers

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libHbdSimpleMonitor.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
HbdSimpleMonitorDict.C: \
  HbdSimpleMonitor.h \
  HbdSimpleMonitorLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

