AUTOMAKE_OPTIONS = foreign

AM_CFLAGS = -Wall -Werror -Wno-error=unused-but-set-variable
AM_CXXFLAGS = ${AM_CFLAGS}

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

include_HEADERS = \
  emcEmbedEvent.h \
  emcEmbedEventv1.h \
  EmcEmbedEventReco.h \
  fkinDumper.h

lib_LTLIBRARIES = \
  libemcEmbed4all.la

libemcEmbed4all_la_SOURCES = \
  Counters.C \
  fkinDumper.C \
  EmcEmbedChecker.C \
  EmcEmbedDriver.C \
  emcEmbedEvent.C \
  EmcEmbedEventReco.C \
  emcEmbedEventv1.C \
  EmcEmbedEventSelector.C \
  EmcEmbedMergedClusterUpdater.C \
  EmcEmbedOutputMover.C \
  EmcEmbedSimDeadTower.C \
  EmcEmbedReader.C \
  EmcEmbedReclusterizer.C \
  EmcEmbedTowerMerger.C \
  EmcEmbedVertexSelector.C \
  SimulationReader.C \
  VertexGetter.C \
  EmcEmbedDict.C

libemcEmbed4all_la_LDFLAGS = \
  -L$(libdir) -L$(OFFLINE_MAIN)/lib \
  `root-config --glibs`
libemcEmbed4all_la_LIBADD = \
  -lemc \
  -lfun4all \
  -lnanoDST \
  -lphool \
  -lSubsysReco \
  -lEG

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libemcEmbed4all.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

EmcEmbedDict.C: \
  EmcEmbedChecker.h \
  EmcEmbedDriver.h \
  EmcEmbedEventSelector.h \
  emcEmbedEvent.h \
  EmcEmbedEventReco.h \
  emcEmbedEventv1.h \
  EmcEmbedMergedClusterUpdater.h \
  EmcEmbedOutputMover.h \
  EmcEmbedReader.h \
  EmcEmbedReclusterizer.h \
  EmcEmbedSimDeadTower.h \
  EmcEmbedVertexSelector.h \
  EmcEmbedTowerMerger.h \
  SimulationReader.h \
  EmcEmbedLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* *dict*

testexternals_SOURCES = testexternals.C





CLEAN_FILES = 
noinst_HEADERS =



BUILT_SOURCES += CopyNonEMCNodes_dict.cc
CLEAN_FILES += CopyNonEMCNodes_dict.h CopyNonEMCNodes_dict.cc
include_HEADERS += CopyNonEMCNodes.h
libemcEmbed4all_la_SOURCES += CopyNonEMCNodes.cc CopyNonEMCNodes_dict.cc
#noinst_HEADERS += CopyNonEMCNodes.h
#libemcEmbed4all_la_SOURCES += CopyNonEMCNodes.cc



BUILT_SOURCES += EmcUnclusterizer_dict.cc
CLEAN_FILES += EmcUnclusterizer_dict.h EmcUnclusterizer_dict.cc
include_HEADERS += EmcUnclusterizer.h
libemcEmbed4all_la_SOURCES += EmcUnclusterizer.cc EmcUnclusterizer_dict.cc



BUILT_SOURCES += EmcDataMerger_dict.cc
CLEAN_FILES += EmcDataMerger_dict.h EmcDataMerger_dict.cc
include_HEADERS += EmcDataMerger.h
libemcEmbed4all_la_SOURCES += EmcDataMerger.cc EmcDataMerger_dict.cc



BUILT_SOURCES += EmcTowerScalerSmearer_dict.cc
CLEAN_FILES += EmcTowerScalerSmearer_dict.h EmcTowerScalerSmearer_dict.cc
include_HEADERS += EmcTowerScalerSmearer.h
libemcEmbed4all_la_SOURCES += EmcTowerScalerSmearer.cc EmcTowerScalerSmearer_dict.cc



BUILT_SOURCES += EmcEmbedDriver2_dict.cc
CLEAN_FILES += EmcEmbedDriver2_dict.h EmcEmbedDriver2_dict.cc
include_HEADERS += EmcEmbedDriver2.h
libemcEmbed4all_la_SOURCES += EmcEmbedDriver2.cc EmcEmbedDriver2_dict.cc









%_dict.cc: %.h
	@echo testing for  `echo $^ | sed 's/\.h/.linkdef.h/'`...
	@if [ -f `echo $^ | sed 's/\.h/.linkdef.h/'` ]; then \
		echo rootcint -f $@ -c -P $(INCLUDES) $^ `echo $^ | sed 's/\.h/.linkdef.h/'`; \
		rootcint -f $@ -c -P $(INCLUDES) $^ `echo $^ | sed 's/\.h/.linkdef.h/'`; \
	else \
		echo rootcint -f $@ -c -P $(INCLUDES) $^+; \
		rootcint -f $@ -c -P $(INCLUDES) $^+; \
	fi
