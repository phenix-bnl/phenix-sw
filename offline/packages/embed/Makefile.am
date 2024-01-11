AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

# List of shared libraries to produce
lib_LTLIBRARIES = libembed.la 

noinst_HEADERS = \
  EmbedmoduleLinkDef.h

include_HEADERS = \
  PHEmbedMcRecoTrack.hh \
  PHEmbededEvent.hh \
  PHEmbedMCEvaluator.hh \
  PHEmbedHistogrammer.hh\
  DchMixer.hh\
  PadMixer.hh\
  TecMixer.hh\
  TofMixer.hh\
  CrkMixer.hh\
  EmcMixer.hh\
  AccMixer.hh\
  TofwMixer.hh\
  VtxMixer.hh

libembed_la_SOURCES = \
  PHEmbedMcRecoTrack.C \
  PHEmbededEvent.C \
  PHEmbedStat.C \
  PHEmbedMCEvaluator.C \
  PHEmbedHistogrammer.C \
  DchMixer.C\
  PadMixer.C\
  TecMixer.C\
  TofMixer.C\
  CrkMixer.C\
  EmcMixer.C\
  AccMixer.C\
  TofwMixer.C\
  VtxMixer.C\
  Embedmodule_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libembed_la_LIBADD = \
  -lCrkPID \
  -lWrappers \
  -lacc \
  -lcrk \
  -ldch \
  -ldgo \
  -lemc \
  -lgea \
  -lpad \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -ltec \
  -ltof \
  -ltofw \
  -luti 

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libembed.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
Embedmodule_Dict.C: \
  PHEmbedMcRecoTrack.hh \
  PHEmbededEvent.hh \
  PHEmbedMCEvaluator.hh \
  PHEmbedHistogrammer.hh\
  DchMixer.hh\
  PadMixer.hh\
  TecMixer.hh\
  TofMixer.hh\
  CrkMixer.hh\
  EmcMixer.hh\
  AccMixer.hh\
  TofwMixer.hh\
  VtxMixer.hh\
  EmbedmoduleLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	 rm -f *Dict*



testexternals_SOURCES = testexternals.C
