
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuonqa.la

AM_LDFLAGS = -L$(libdir) `root-config --evelibs` -L${OFFLINE_MAIN}/lib

AM_CXXFLAGS = `root-config --cflags`

INCLUDES = -I$(includedir) -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include -I$(OPT_PHENIX)/include

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
	MuonQAAnalyzer.h \
	MuonQAFlags.h \
	SubsystemQA.h \
	HistoQABase.h \
	HistoQA.h \
	HistoGroupQA.h \
	ObservableQABase.h \
	ObservableQA.h \
	ObservableFactory.cxx \
	ActiveWedgeQA.h \
	EntriesQA.h \
	MeanQA.h \
	PeakQA.h \
	UtilityQA.h

##############################################
# please add new classes in alphabetical order

libmuonqa_la_SOURCES = \
	MuonQAAnalyzer.cxx \
	SubsystemQA.cxx \
	HistoQABase.cxx \
	HistoQA.cxx \
	HistoGroupQA.cxx \
	ObservableQABase.cxx \
	ObservableFactory.cxx \
	EntriesQA.cxx \
	MeanQA.cxx \
	PeakQA.cxx \
	ActiveWedgeQA.cxx \
	MuonQAAnalyzerDict.C

libmuonqa_la_LIBADD = -lboost_filesystem -lboost_regex -lodbc++ -lSpectrum

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmuonqa.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

MuonQAAnalyzerDict.C: \
	MuonQAAnalyzer.h \
	MuonQAFlags.h \
	SubsystemQA.h \
	HistoQABase.h \
	HistoGroupQA.h \
	HistoQA.h \
	ObservableQABase.h \
	EntriesQA.h \
	MeanQA.h \
	PeakQA.h \
	ActiveWedgeQA.h \
	MuonQAAnalyzerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^
