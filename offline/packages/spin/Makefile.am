AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libspin.la

bin_PROGRAMS = spinDB

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

noinst_HEADERS = \
  SpinEventLinkDef.h \
  SpinEventGL1pLinkDef.h \
  SpinDataEventOutLinkDef.h


include_HEADERS = \
  SpinEvent.h \
  SpinEventGL1p.h \
  SpinEventConstants.h \
  SpinDataEventOut.h \
  SpinDataEventOutv1.h \
  SpinDataEventOutv2.h

libspin_la_LDFLAGS = \
  -L$(libdir) -L$(OFFLINE_MAIN)/lib \
  `root-config --glibs`

libspin_la_LIBADD = \
  -lheader \
  -lpdbcalBase \
  -lphool

libspin_la_SOURCES = \
  SpinEvent.C \
  SpinEventDict.C \
  SpinEventGL1p.C \
  SpinEventGL1pDict.C \
  SpinDataEventOut.C \
  SpinDataEventOutDict.C \
  SpinDataEventOutv1.C \
  SpinDataEventOutv2.C

# Rule for generating spinDB
spinDB_SOURCES = spinDB.cc

spinDB_LDFLAGS = \
 -L$(libdir) -L$(OFFLINE_MAIN)/lib \
 `root-config --glibs`
spinDB_LDADD = \
  -lpdbcalBase \
  -lphool \
  -lPgCal \
  -lphtime  \
  -lphgeo \
  -lEvent


noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libspin.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
SpinDataEventOutDict.C : SpinDataEventOut.h SpinDataEventOutv1.h SpinDataEventOutv2.h SpinDataEventOutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

SpinEventDict.C : SpinEvent.h SpinEventLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

SpinEventGL1pDict.C : SpinEventGL1p.h SpinEventGL1pLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

# Rule for generating CINT dictionaries from class headers.
%Dict.C %Dict.h: %.hh
	rootcint -f $*Dict.C -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $*.hh

clean-local:
	 rm -f $(BUILT_SOURCES)

