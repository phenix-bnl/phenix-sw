AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libSvxDstQA.la

include_HEADERS = \
  SvxDstQA.h \
  SvxQAForProduction.h \
  SvxFillHisto.h \
  SvxFillHistoPixel.h \
  SvxFillHistoStrip.h \
  SvxFillHistoBeampro.h \
  SvxStripEventQA.h \
  SvxAlignment_QA.h \
  SvxQAInfo.h \
  SvxQAInfov1.h \
  SvxRpEventQA.h \
  SvxStripClusterQA.h \
  SvxCntQA.h \
  SvxDaqQA.h \
  SvxStabilityQA.h \
  SvxClusterStabilityQA.h \
  SvxEfficiencyQA.h \
  SvxQAEventSelection.h \
  MisalignSearchHist.h \
  MisalignSearchTree.h

noinst_HEADERS = \
  SvxQADefs.h \
  SvxDstQALinkDef.h

libSvxDstQA_la_SOURCES = \
  SvxDstQA.C \
  SvxQAForProduction.cc \
  SvxFillHistoPixel.C \
  SvxFillHistoStrip.C \
  SvxFillHistoBeampro.C \
  SvxStripEventQA.C \
  SvxAlignment_QA.cc \
  SvxQAInfov1.C \
  SvxRpEventQA.C \
  SvxStripClusterQA.C \
  SvxCntQA.cc \
  SvxDaqQA.C \
  SvxStabilityQA.C \
  SvxClusterStabilityQA.C \
  SvxEfficiencyQA.C \
  SvxQAEventSelection.C \
	MisalignSearchHist.C \
	MisalignSearchTree.C

nodist_libSvxDstQA_la_SOURCES = SvxDstQADict.C


libSvxDstQA_la_LIBADD = -L$(libdir) -L$(OFFLINE_MAIN)/lib  -lphool -lfun4all -lsvx -lreactionplane -lsimreco

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libSvxDstQA.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

SvxDstQADict.C: \
  SvxDstQA.h \
  SvxQAForProduction.h \
  SvxFillHisto.h \
  SvxFillHistoPixel.h \
  SvxFillHistoStrip.h \
  SvxFillHistoBeampro.h \
  SvxStripEventQA.h \
  SvxAlignment_QA.h \
  SvxQAInfo.h \
  SvxQAInfov1.h \
  SvxRpEventQA.h \
  SvxStripClusterQA.h \
  SvxCntQA.h \
  SvxDaqQA.h \
  SvxStabilityQA.h \
  SvxClusterStabilityQA.h \
  SvxEfficiencyQA.h \
  SvxQAEventSelection.h \
  MisalignSearchHist.h \
  MisalignSearchTree.h \
  SvxDstQALinkDef.h

	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
