AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

# Allways put -I$(includedir) first to pick up includes from your own install area first
INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libDchAlphaPhi.la

libDchAlphaPhi_la_SOURCES = \
  DchAlphaPhi.C \
  DchAlphaPhi_Dict.C 

libDchAlphaPhi_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -ldch \
  -lfun4all \
  -lnanoDST \
  -lphool \
  -lSubsysReco 

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libDchAlphaPhi.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
DchAlphaPhi_Dict.C: \
  DchAlphaPhi.h \
  DchAlphaPhiLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
