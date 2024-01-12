AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = -D_REENTRANT

INCLUDES = \
  -I$(includedir) \
  -I$(srcdir)/dimuons \
  -I$(srcdir)/tools \
  -I$(srcdir)/singlemuons \
  -I$(srcdir)/newsinglemuons \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include 



lib_LTLIBRARIES = \
  libMWGpico.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libMWGpico_la_LIBADD = \
  -lBbcMultipleVtx \
  -lMWG_interface \
  -lMWG_tools \
  -lmutrg \
  -lreactionplane \
  -lspin \
  -lMWGOO

include_HEADERS = \
  MWGpico.h \
  MWGCuts.h \
  MWGRunInfo.h 



libMWGpico_la_SOURCES = \
  MWGpico.C \
  MWGCuts.C \
  MWGRunInfo.C \
  LoadNano.C \
  dimuons/Dimuons.C \
  dimuons/DimuonsRun3.C \
  dimuons/DimuonsRun4.C \
  dimuons/DimuonsRun5.C \
  dimuons/DimuonsRun6.C \
  dimuons/DimuonsRun7.C \
  dimuons/DimuonsRun8.C \
  dimuons/DimuonsRun8pp.C \
  dimuons/DimuonsRun9.C \
  dimuons/DimuonsRun10.C \
  dimuons/DimuonsRun11.C \
  dimuons/DimuonsRun11pp.C \
  dimuons/DimuonsRun12pp.C \
  dimuons/DimuonsRun12.C \
  dimuons/EvtMix.C \
  dimuons/EvtMixRun3.C \
  dimuons/EvtMixRun4.C \
  newsinglemuons/NewSngmuons.C\
  newsinglemuons/NewSngmuonsRun9.C\
  newsinglemuons/NewSngmuonsRun11.C\
  newsinglemuons/NewSngmuonsRun12.C\
  singlemuons/Sngmuons.C\
  singlemuons/SngmuonsRun3.C\
  singlemuons/SngmuonsRun4.C\
  singlemuons/SngmuonsRun5.C\
  singlemuons/SngmuonsRun6.C\
  singlemuons/SngmuonsRun7.C\
  singlemuons/SngmuonsRun8.C\
  singlemuons/SngmuonsRun8pp.C\
  singlemuons/SngmuonsRun9.C\
  singlemuons/SngmuonsRun12pp200.C\
  singlemuons/SngmuonsRun12CuAu.C\
  singlemuons/SngmuonsRun13.C

nodist_libMWGpico_la_SOURCES = \
  MWGpico_Dict.C \
  MWGCuts_Dict.C \
  MWGRunInfo_Dict.C

noinst_HEADERS = \
  MWGCutsLinkDef.h \
  MWGRunInfoLinkDef.h \
  MWGpicoLinkDef.h

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libMWGpico.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
%_Dict.C:  %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
