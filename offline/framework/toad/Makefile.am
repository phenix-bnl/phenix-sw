AUTOMAKE_OPTIONS = foreign

INCLUDES := -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = libTOAD.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libTOAD_la_LDFLAGS = \
  `root-config --libs`

noinst_HEADERS = TOADLinkDef.h

include_HEADERS =    \
  TOAD.h

libTOAD_la_SOURCES = \
  TOAD.C       \
  TOAD_Dict.C

BUILT_SOURCES = testexternals.C
noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C

testexternals_LDADD = \
  libTOAD.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
TOAD_Dict.C:   \
  TOAD.h       \
  TOADLinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^


clean-local:
	rm -f *Dict*
