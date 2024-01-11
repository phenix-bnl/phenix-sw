AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libEWG.la

noinst_HEADERS = \
  PHSnglElectronv1.h \
  PHElectronLinkDef.h  \
  EWGnanoLinkDef.h

include_HEADERS = \
  PHElectronv1.h \
  EWGInclusiveNanoCutsv1.h \
  EWGInclusiveNanoCutsv2.h \
  CONVInclusiveNanoCutsv2.h \
  EWGnanoCuts.h \
  HBDMiniCellReset.h


libEWG_la_SOURCES = \
 PHElectronv1.C \
 PHSnglElectronv1.C \
 PHElectron_Dict.C \
 EWGInclusiveNanoCutsv1.C \
 EWGInclusiveNanoCutsv2.C \
 CONVInclusiveNanoCutsv2.C \
 EWGnanoCuts.C \
 EWGnano_Dict.C \
 HBDMiniCellReset.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libEWG_la_LIBADD = \
  -lhbd \
  -lnanoDST \
  -lphool

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libEWG.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.

PHElectron_Dict.C: PHElectronv1.h PHSnglElectronv1.h PHElectronLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

EWGnano_Dict.C: EWGnanoCuts.h EWGInclusiveNanoCutsv1.h EWGInclusiveNanoCutsv2.h CONVInclusiveNanoCutsv2.h HBDMiniCellReset.h EWGnanoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
