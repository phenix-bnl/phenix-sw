
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libfvtxoo_modules.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib

AM_CXXFLAGS = -Wall -Werror

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include 

libfvtxoo_modules_la_LIBADD = \
  -lfvtxgeom \
  -lfvtxoo_interface \
  -lmuioo_interface \
  -lmutoo_modules \
  -lmuon_util \
  -lodbc++

##############################################
# please add new classes in alphabetical order

noinst_HEADERS = \
  Par_LinkDef.h

include_HEADERS = \
  mFvtxAddVtxHits.h \
  mFvtxEmbed.h \
  mFvtxEmbedSili.h \
  mFvtxEval.h \
  mFvtxEmbedPar.h \
  mFvtxEvalPar.h \
  mFvtxFastSim.h \
  mFvtxFastSimPar.h \
  mFvtxFindClus.h \
  mFvtxFindClusPar.h \
  mFvtxFindCoord.h \
  mFvtxFindCoordPar.h \
  mFvtxFindHoughTracks.h \
  mFvtxFindSvxClusters.h \
  mFvtxFindSvxClustersPar.h \
  mFvtxFindTrack.h \
  mFvtxFindTrackPar.h \
  mFvtxFindTracks.h \
  mFvtxFindTrackMC.h \
  mFvtxFindTrackMCPar.h \
  mFvtxKalFit.h \
  mFvtxKalFitPar.h \
  mFvtxKalFitMC.h \
  mFvtxKalFitMCPar.h \
  mFvtxMatch.h \
  mFvtxModuleBase.h \
  mFvtxPackPRDF.h \
  mFvtxPackPRDFPar.h \
  mFvtxQuadraticLineFit.h \
  mFvtxRejectTrack.h \
  mFvtxRejectTrackPar.h \
  mFvtxResponse.h \
  mFvtxResponsePar.h \
  mFvtxSlowSim.h \
  mFvtxSlowSimPar.h \
  mFvtxStraightLineFit.h \
  mFvtxUnpack.h \
  mFvtxUnpackPar.h \
  mMutKalFitWithSili.h \
  mMutKalFitWithSiliPar.h \
  mMutKalFitWithSiliReal.h \
  mMutKalFitWithSiliRealPar.h \
  TFvtxParBase.h

##############################################
# please add new classes in alphabetical order

libfvtxoo_modules_la_SOURCES = \
  mFvtxAddVtxHits.C \
  mFvtxEmbed.cxx \
  mFvtxEmbedSili.cxx \
  mFvtxEval.cxx \
  mFvtxFastSim.cxx \
  mFvtxFindClus.cxx \
  mFvtxFindCoord.cxx \
  mFvtxFindSvxClusters.cxx \
  mFvtxFindHoughTracks.C \
  mFvtxFindTrack.cxx \
  mFvtxFindTrackMC.cxx \
  mFvtxFindTracks.C \
  mFvtxFindTracksDict.C \
  mFvtxKalFit.cxx \
  mFvtxKalFitMC.cxx \
  mFvtxMatch.C \
  mFvtxPackPRDF.cxx \
  mFvtxQuadraticLineFit.C \
  mFvtxRejectTrack.cxx \
  mFvtxResponse.cxx \
  mFvtxSlowSim.cxx \
  mFvtxStraightLineFit.C \
  mFvtxUnpack.cxx \
  mMutKalFitWithSili.cxx \
  mMutKalFitWithSiliReal.cxx \
  TFvtxParBase.C \
  mFvtxEmbedPar.C \
  mFvtxEvalPar.C \
  mFvtxFastSimPar.C \
  mFvtxFindClusPar.C \
  mFvtxFindCoordPar.C \
  mFvtxFindSvxClustersPar.C \
  mFvtxFindTrackMCPar.C \
  mFvtxFindTrackPar.C \
  mFvtxKalFitMCPar.C \
  mFvtxKalFitPar.C \
  mFvtxPackPRDFPar.C \
  mFvtxRejectTrackPar.C \
  mFvtxResponsePar.C \
  mFvtxSlowSimPar.C \
  mFvtxUnpackPar.C \
  mMutKalFitWithSiliPar.C \
  mMutKalFitWithSiliRealPar.C \
  Par_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libfvtxoo_modules.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

Par_Dict.C: \
  TFvtxParBase.h \
  mFvtxEmbedPar.h  \
  mFvtxEvalPar.h \
  mFvtxFastSimPar.h \
  mFvtxFindClusPar.h \
  mFvtxFindCoordPar.h \
  mFvtxFindSvxClustersPar.h \
  mFvtxFindTrackMCPar.h \
  mFvtxFindTrackPar.h \
  mFvtxKalFitMCPar.h \
  mFvtxKalFitPar.h \
  mFvtxPackPRDFPar.h \
  mFvtxRejectTrackPar.h \
  mFvtxResponsePar.h \
  mFvtxSlowSimPar.h \
  mFvtxUnpackPar.h \
  mMutKalFitWithSiliPar.h \
  mMutKalFitWithSiliRealPar.h \
  Par_LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C : %.h %LinkDef.h
	rootcint -f $@ -c -D__linux__ $(DEFAULT_INCLUDES) $(INCLUDES) $^
