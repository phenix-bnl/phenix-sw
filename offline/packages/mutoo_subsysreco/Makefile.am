AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include -I$(OPT_PHENIX)/include

lib_LTLIBRARIES = \
  libSubsysReco_muons.la \
  libmuon_util.la \
  libmutoo_subsysreco.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -lodbc++

libSubsysReco_muons_la_LIBADD = \
  -lSubsysReco \
  -lRawDataCheck \
  -lmutoo_core

libmuon_util_la_LIBADD = \
  -lPHGeant \
  libSubsysReco_muons.la \
  -lmuigeom \
  -ltrigger \
  -lmutoo_classes \
  -luti

libmutoo_subsysreco_la_LIBADD = \
  libmuon_util.la \
  -lmutoo_modules \
  -lmutoo_alignment \
  -lmuioo_modules \
  -lrpcoo_modules

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  MuonSubsysReco.h \
  MuonTrigFilter.h \
  MuonUtil.h

noinst_HEADERS = \
  MuidEffic.h \
  MuiooReco.h \
  MuonAlign.h \
  MuonAlignDev.h \
  MuonAnaTuples.h \
  MuonArmSelect.h \
  MuonCloseTFiles.h \
  MuonCounter.h \
  MuonDev.h \
  MuonDisalign.h \
  MuonDumpMap.h \
  MuonEventNumberFilter.h \
  MuonGlobalAlign.h \
  MuonPackPRDF.h \
  MuonReadbackDST.h \
  MuonResources.h \
  MuonUnpackDST.h \
  MuonUnpackPisa.h \
  MuonUnpackPRDF.h \
  MuonUnpackSim.h \
  MuonRotateGapCoord.h \
  MuonUnpackSlowSimDst.h \
  MutooHitEval.h \
  MutooClusterEval.h \
  MutooResEval.h \
  libmuon_util_LinkDef.h \
  libmutoo_subsysreco_LinkDef.h \
  MuonSubsysRecoLinkDef.h

##############################################
# please add new classes in alphabetical order
libSubsysReco_muons_la_SOURCES = \
  MuonSubsysReco.cxx
nodist_libSubsysReco_muons_la_SOURCES = MuonSubsysRecoDict.C

libmuon_util_la_SOURCES = \
  MuonCounter.cxx \
  MuonUtil.cxx
nodist_libmuon_util_la_SOURCES = libmuon_util_Dict.C

libmutoo_subsysreco_la_SOURCES = \
  MuidEffic.cxx \
  MuiooReco.cxx \
  MuonAlign.cxx \
  MuonAlignDev.cxx \
  MuonAnaTuples.cxx \
  MuonArmSelect.cxx \
  MuonCloseTFiles.cxx \
  MuonDev.cxx \
  MuonDisalign.cxx \
  MuonDumpMap.cxx \
  MuonEventNumberFilter.cxx \
  MuonGlobalAlign.cxx \
  MuonPackPRDF.cxx \
  MuonReadbackDST.cxx \
  MuonResources.cxx \
  MuonTrigFilter.cxx \
  MuonUnpackDST.cxx \
  MuonUnpackPisa.cxx \
  MuonUnpackPRDF.cxx \
  MuonUnpackSim.cxx \
  MuonRotateGapCoord.cxx \
  MuonUnpackSlowSimDst.cxx \
  MutooClusterEval.cxx \
  MutooHitEval.cxx \
  MutooResEval.cxx
nodist_libmutoo_subsysreco_la_SOURCES = libmutoo_subsysreco_Dict.C

noinst_PROGRAMS = \
  testexternals_subsysreco \
  testexternals_util \
  testexternals

testexternals_subsysreco_SOURCES = testexternals.C
testexternals_subsysreco_LDADD = libSubsysReco_muons.la

testexternals_util_SOURCES = testexternals.C
testexternals_util_LDADD = libmuon_util.la

testexternals_SOURCES = testexternals.C
testexternals_LDADD =  libmutoo_subsysreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# rules to make root dictionaries
libmuon_util_Dict.C: \
  MuonCounter.h \
  MuonUtil.h \
  libmuon_util_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

libmutoo_subsysreco_Dict.C: \
  MuidEffic.h \
  MuiooReco.h \
  MuonAlign.h \
  MuonAlignDev.h \
  MuonAnaTuples.h \
  MuonArmSelect.h \
  MuonCloseTFiles.h \
  MuonDev.h \
  MuonDisalign.h \
  MuonDumpMap.h \
  MuonEventNumberFilter.h \
  MuonGlobalAlign.h \
  MuonPackPRDF.h \
  MuonReadbackDST.h \
  MuonResources.h \
  MuonTrigFilter.h \
  MuonUnpackDST.h \
  MuonUnpackPisa.h \
  MuonUnpackPRDF.h \
  MuonUnpackSim.h \
  MuonRotateGapCoord.h \
  MuonUnpackSlowSimDst.h \
  MutooClusterEval.h \
  MutooHitEval.h \
  MutooResEval.h \
  libmutoo_subsysreco_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

