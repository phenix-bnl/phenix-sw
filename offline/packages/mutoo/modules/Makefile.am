# $Id: Makefile.am,v 1.66 2008/07/30 12:03:36 hpereira Exp $
AUTOMAKE_OPTIONS = foreign

AM_CXXFLAGS = 

lib_LTLIBRARIES = libmutoo_modules.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmutoo_modules_la_LIBADD = \
 -lmutoo_classes \
 -lmuioo_interface
 
noinst_HEADERS = 

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  mMutBPFit.h \
  mMutBPFitPar.h \
  mMutBPVertex.h \
  mMutBPVertexPar.h \
  mMutCalibrate.h \
  mMutCalibratePar.h \
  mMutEmbed.h \
  mMutEmbedPar.h \
  mMutEval.h \
  mMutEvalPar.h \
  mMutFindClus.h \
  mMutFindClusPar.h \
  mMutFindGapCoord.h \
  mMutFindGapCoordPar.h \
  mMutFindStub.h \
  mMutFindStubPar.h \
  mMutFindTrack.h \
  mMutFindTrackPar.h \
  mMutFindTrackMC.h \
  mMutFindTrackMCPar.h \
  mMutFindVtx.h \
  mMutFindVtxPar.h \
  mMutFitClus.h \
  mMutFitClusPar.h \
  mMutFitVtx.h \
  mMutFitVtxPar.h \
  mMutKalFit.h \
  mMutKalFitPar.h \
  mMutMatchCoord.h \
  mMutMatchCoordPar.h \
  mMutMuiRoad.h \
  mMutMuiRoadPar.h \
  mMutPackPRDF.h \
  mMutPackPRDFPar.h \
  mMutRejectTrack.h \
  mMutRejectTrackPar.h \
  mMutResponse.h \
  mMutResponsePar.h \
  mMutSlowSim.h \
  mMutSlowSimPar.h \
  mMutStraightFit.h \
  mMutStraightFitPar.h \
  mMutStripOccup.h \
  mMutStripOccupPar.h \
  mMutStubFit.h \
  mMutStubFitPar.h \
  mMutUnpack.h \
  mMutUnpackPar.h \
  mMutZeroSup.h \
  mMutZeroSupPar.h \
  TMutParBase.h

##############################################
# please add new classes in alphabetical order

libmutoo_modules_la_SOURCES = \
  mMutBPFit.cxx \
  mMutBPVertex.cxx \
  mMutCalibrate.cxx \
  mMutEmbed.cxx \
  mMutEval.cxx \
  mMutFindClus.cxx \
  mMutFindGapCoord.cxx \
  mMutFindStub.cxx \
  mMutFindTrack.cxx \
  mMutFindTrackMC.cxx \
  mMutFindVtx.cxx \
  mMutFitClus.cxx \
  mMutFitVtx.cxx \
  mMutKalFit.cxx \
  mMutMatchCoord.cxx \
  mMutMuiRoad.cxx \
  mMutPackPRDF.cxx \
  mMutRejectTrack.cxx \
  mMutResponse.cxx \
  mMutSlowSim.cxx \
  mMutStraightFit.cxx \
  mMutStripOccup.cxx \
  mMutStubFit.cxx \
  mMutUnpack.cxx \
  mMutZeroSup.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmutoo_modules.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
