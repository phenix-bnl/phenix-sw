AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libfvtx_subsysreco.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(ROOTSYS)/lib

libfvtx_subsysreco_la_LIBADD = \
  -lMinuit2 \
  -lSubsysReco_muons \
  -lfvtxoo_modules  \
  -lfvtxoo_alignment \
  -lmutoo_modules \
  -lmutoo_alignment \
  -lPHPythia \
  -lBbcMultipleVtx \
  -lvtx

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  FvtxAlign.h \
  FvtxDisalign.h \
  FvtxEval.h \
  FvtxEventFilter.h \
  FvtxFastSim.h \
  FvtxGlobalAlign.h \
  FvtxMCEval.h \
  FvtxMCMatch.h \
  FvtxPackPRDF.h \
  FvtxPrimVertex.h \
  FvtxPrimVtxEval.h \
  FvtxReadbackDST.h \
  FvtxReco.h \
  FvtxRecoMC.h \
  FvtxRecoWithMut.h \
  FvtxTrigEmulator.h \
  FvtxUnpackDST.h \
  FvtxUnpackPisa.h \
  FvtxUnpackPRDF.h \
  FvtxUnpackSim.h \
  FvtxVertexTest.h 

##############################################
# please add new classes in alphabetical order

libfvtx_subsysreco_la_SOURCES = \
  FvtxAlign.cxx \
  FvtxDisalign.cxx \
  FvtxEval.cxx \
  FvtxEventFilter.cxx \
  FvtxFastSim.cxx \
  FvtxGlobalAlign.cxx \
  FvtxMCEval.cxx \
  FvtxMCMatch.C \
  FvtxPackPRDF.cxx \
  FvtxPrimVertex.cxx \
  FvtxPrimVtxEval.C \
  FvtxReadbackDST.C \
  FvtxReco.cxx \
  FvtxRecoMC.cxx \
  FvtxRecoWithMut.cxx \
  FvtxTrigEmulator.C \
  FvtxUnpackDST.cxx \
  FvtxUnpackPisa.cxx \
  FvtxUnpackPRDF.cxx \
  FvtxUnpackSim.cxx \
  FvtxVertexTest.cxx

nodist_libfvtx_subsysreco_la_SOURCES = \
  FvtxAlignDict.C \
  FvtxDisalignDict.C \
  FvtxEvalDict.C \
  FvtxEventFilterDict.C \
  FvtxFastSimDict.C \
  FvtxGlobalAlignDict.C \
  FvtxMCEvalDict.C \
  FvtxMCMatchDict.C \
  FvtxPackPRDFDict.C \
  FvtxPrimVertexDict.C \
  FvtxPrimVtxEvalDict.C \
  FvtxReadbackDSTDict.C \
  FvtxRecoDict.C \
  FvtxRecoMCDict.C \
  FvtxRecoWithMutDict.C \
  FvtxTrigEmulatorDict.C \
  FvtxUnpackDSTDict.C \
  FvtxUnpackPisaDict.C \
  FvtxUnpackPRDFDict.C \
  FvtxUnpackSimDict.C \
  FvtxVertexTestDict.C

noinst_HEADERS = \
  FvtxAlignLinkDef.h \
  FvtxDisalignLinkDef.h \
  FvtxEvalLinkDef.h \
  FvtxEventFilterLinkDef.h \
  FvtxFastSimLinkDef.h \
  FvtxGlobalAlignLinkDef.h \
  FvtxMCEvalLinkDef.h \
  FvtxMCMatchLinkDef.h \
  FvtxPackPRDFLinkDef.h \
  FvtxPrimVertexLinkDef.h \
  FvtxPrimVtxEvalLinkDef.h \
  FvtxReadbackDSTLinkDef.h \
  FvtxRecoLinkDef.h \
  FvtxRecoMCLinkDef.h \
  FvtxRecoWithMutLinkDef.h \
  FvtxTrigEmulatorLinkDef.h \
  FvtxUnpackDSTLinkDef.h \
  FvtxUnpackPisaLinkDef.h \
  FvtxUnpackPRDFLinkDef.h \
  FvtxUnpackSimLinkDef.h \
  FvtxVertexTestLinkDef.h

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libfvtx_subsysreco.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# rules to make root dictionaries
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

testexternals_SOURCES = testexternals.C
