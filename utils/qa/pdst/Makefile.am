AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = \
  libdstqa_common.la \
  libdstqa_central.la \
  libdstqa_muons.la \
  libdstqa.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libdstqa_common_la_LIBADD = \
  -ltrigger \
  -lbbc \
  -lzdc \
  -lfun4all

libdstqa_central_la_LIBADD = \
  libdstqa_common.la \
  -lspin \
  -lert \
  -lfcl \
  -lcrk

libdstqa_muons_la_LIBADD = \
  libdstqa_common.la \
  -lmuioo_interface \
  -lmutoo_interface \
  -lfvtxoo_interface  \
  -lfvtxoo_modules  \
  -ltrigger \
  -lfun4all

libdstqa_la_LIBADD = \
  libdstqa_common.la \
  libdstqa_central.la \
  libdstqa_muons.la

noinst_HEADERS = \
  dstqa_commonLinkDef.h \
  dstqa_centralLinkDef.h \
  dstqa_muonsLinkDef.h \
  histBbc.h \
  histDch.h \
  histPad.h \
  histTof.h \
  histZdc.h \
  histErt.h \
  histFcl.h \
  histSpin.h \
  histElectron.h \
  QAMui.h \
  QAMut.h \
  QAFvtx.h \
  summaryQA.h \
  summaryQALinkDef.h

include_HEADERS = \
 QaBankID.h \
 QaEntry.h \
 QaDatabaseManager.h

libdstqa_common_la_SOURCES = \
  histBbc.C \
  histZdc.C \
  dstqa_common_dict.C

libdstqa_central_la_SOURCES = \
  histDch.C \
  histPad.C \
  histTof.C \
  histFcl.C \
  histErt.C \
  histSpin.C \
  histElectron.C \
  PhysicsqaReco.C \
  QaDatabaseManager.C \
  dstqa_central_dict.C

libdstqa_muons_la_SOURCES = \
  QAMui.C \
  QAMut.C \
  QAFvtx.C \
  dstqa_muons_dict.C

# note libdstqa has no sources and should be left so.
# this is a "meta" library, that loads all specialization
# namely dstqa_common, dstqa_central and dstqa_muons
# please put new sources under the relevant library amongst the above.
libdstqa_la_SOURCES =


noinst_PROGRAMS = \
  testexternals_common \
  testexternals_central \
  testexternals_muons

testexternals_common_SOURCES = testexternals.C
testexternals_common_LDADD = libdstqa_common.la

testexternals_central_SOURCES = testexternals.C
testexternals_central_LDADD = libdstqa_central.la

testexternals_muons_SOURCES = testexternals.C
testexternals_muons_LDADD = libdstqa_muons.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

dstqa_common_dict.C : \
  histBbc.h \
  histZdc.h \
  dstqa_commonLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

dstqa_central_dict.C : \
  histPad.h \
  histTof.h \
  histDch.h \
  histErt.h \
  histFcl.h \
  histSpin.h \
  histElectron.h \
  PhysicsqaReco.h \
  QaDatabaseManager.h \
  dstqa_centralLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

dstqa_muons_dict.C : \
  QAMut.h \
  QAMui.h \
  QAFvtx.h \
  dstqa_muonsLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

bin_SCRIPTS = qa_check

clean-local:
	rm -f *Dict.*

