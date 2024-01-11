# $Id: Makefile.am,v 1.23 2018/07/04 19:16:43 slash Exp $

AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmutoo_core.la libmutoo_base.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =-I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmutoo_core_la_LIBADD = \
  -lbbc \
  -lgea \
  -lgsl \
  -lgslcblas

libmutoo_base_la_LIBADD = \
  libmutoo_core.la \
  -lmutgeom \
  -lmutgeom_fortran

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  MUTOO.h \
  MUTOO_ROTATION.h \
  MUTOO_DIGIT.h \
  MUTOO_FEM.h \
  MUTOO_HASH_MAP.h \
  PHClassId.hh \
  PHConstKeyIterator.h \
  PHConstMapIterator.h \
  PHException.h \
  PHGslMatrix.h \
  PHGslRng.h \
  PHIOArray.hh \
  PHIOArray_v1.hh \
  PHKey.hh \
  PHKeyIterator.h \
  PHMap.h \
  PHMapBase.h \
  PHMapBaseT.h \
  PHMapIterator.h \
  PHMapManager.h \
  PHMapFinder.h \
  PHMapFinderBase.h \
  PHNonStaticMapFinder.h \
  PHResources.h \
  PHTFileServer.h \
  PHTrackIntegratorKF.h \
  TMutActivePacketSet.h \
  TMutBPPar.hh \
  TMutCathErrors.h \
  TMutChargeCorrection.h \
  TMutExtVtx.h \
  TMutFitCathodeSamples.h \
  TMutFitPar.hh \
  TMutGeo.h \
  TMutKalmanUtil.h \
  TMutKalmanFilter.h \
  TMutKeyGen.h \
  TMutMapIO.h \
  TMutMSCovar.h \
  TMutNode.h \
  TMutParameterDB.h \
  TMutRecoPar.hh \
  TMutTrkPar.hh \
  TMutTrackUtil.h \
  TMutVertexFit.h \
  TMutVtxPar.hh \
  TMutVtxPar_v2.hh


##############################################
# please add new classes in alphabetical order

libmutoo_core_la_SOURCES = \
  MUTOO.cxx \
  MUTOODict.C \
  PHClassId.cxx \
  PHGslMatrix.cxx \
  PHGslRng.cxx \
  PHIOArray_v1.cxx \
  PHIOArray_v1Dict.C \
  PHIOArray.cxx\
  PHIOArrayDict.C \
  PHKey.cxx \
  PHKeyDict.C \
  PHMapBase.cxx \
  PHMapFinderBase.cxx \
  PHMapManager.cxx \
  PHMapManagerDict.C \
  PHResources.cxx \
  PHTFileServer.cxx \
  TMutExtVtx.cxx \
  TMutExtVtxDict.C \
  TMutParameterDB.cxx \
  TMutParameterDBDict.C

libmutoo_base_la_SOURCES = \
  PHTrackIntegratorKF.cxx \
  TMutBPPar.cxx \
  TMutBPParDict.C \
  TMutCathErrors.cxx \
  TMutChargeCorrection.cxx \
  TMutChargeCorrectionDict.C\
  TMutFitCathodeSamples.cxx \
  TMutFitPar.cxx \
  TMutFitParDict.C \
  TMutGeo.cxx \
  TMutKalmanFilter.cxx \
  TMutKalmanUtil.cxx \
  TMutKeyGen.cxx \
  TMutRecoPar.cxx \
  TMutRecoParDict.C \
  TMutTrkPar.cxx \
  TMutTrkParDict.C \
  TMutTrackUtil.cxx \
  TMutVertexFit.cxx \
  TMutVtxPar.cxx \
  TMutVtxParDict.C \
  TMutVtxPar_v2.cxx \
  TMutVtxPar_v2Dict.C

noinst_PROGRAMS = \
  testexternals_core \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmutoo_base.la

testexternals_core_SOURCES = \
  testexternals.C

testexternals_core_LDADD = \
  libmutoo_core.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# rules for root dictionaries
%Dict.C: %.hh
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHKeyDict.C: PHKey.hh PHKeyLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
