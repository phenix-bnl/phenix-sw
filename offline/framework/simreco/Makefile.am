AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libsimreco_base.la \
  libsimreco.la 

libsimreco_base_la_SOURCES = \
  Fun4AllPisaInputManager.C \
  Fun4AllPisaInputManagerDict.C \
  HeadSimreco.C \
  HeadSimrecoDict.C \
  SIMRECO.C \
  TrigSimreco.C \
  TrigSimrecoDict.C \
  T0Simreco.C \
  T0SimrecoDict.C \
  VtxSimreco.C \
  VtxSimrecoDict.C

libsimreco_la_SOURCES = \
  AccSimreco.C \
  AccclusterSimreco.C \
  BbcSimreco.C \
  CrkSimreco.C \
  DchSimreco.C \
  EmcSimreco.C \
  ErtSimreco.C \
  EvaSimreco.C \
  McEvalSimreco.C \
  MuPCSimreco.C \
  PadSimreco.C \
  PrdfSimreco.C \
  TecSimreco.C \
  Simreco_Dict.C \
  SyncSimreco.C \
  TecpidSimreco.C \
  TofSimreco.C \
  NCCSimreco.C \
  ZdcSimreco.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libsimreco_base_la_LIBADD = \
  -lfun4allfuncs_common

libsimreco_la_LIBADD = \
  libsimreco_base.la \
  -lcteval \
  -lemc-simulation \
  -lert \
  -llvl2 \
  -lmupc \
  -lgea \
  -ltecpid

noinst_PROGRAMS = \
  testexternals \
  testexternals_base

testexternals_base_SOURCES = testexternals.C
testexternals_base_LDADD = libsimreco_base.la

testexternals_SOURCES = testexternals.C
testexternals_LDADD = libsimreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
Simreco_Dict.C: \
  AccSimreco.h \
  AccclusterSimreco.h \
  BbcSimreco.h \
  CrkSimreco.h \
  DchSimreco.h \
  EmcSimreco.h \
  ErtSimreco.h \
  EvaSimreco.h \
  McEvalSimreco.h \
  MuPCSimreco.h \
  PadSimreco.h \
  PrdfSimreco.h \
  SyncSimreco.h \
  TecSimreco.h \
  TecpidSimreco.h \
  TofSimreco.h \
  ZdcSimreco.h \
  NCCSimreco.h \
  SimrecoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

### general root dictionary generation (including LinkDef)
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

