AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libdch.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(ROOTSYS)/lib
AM_CXXFLAGS = -Wall -Werror

libdch_la_LIBADD = \
  -lSubsysReco \
  -lWrappers \
  -ldgo \
  -lfun4all \
  -lgea \
  -lMinuit \
  -lpad \
  -lpdbcalBase \
  -lphflag \
  -lphgeo \
  -lphool \
  -lphtime \
  -luti

noinst_HEADERS = \
  mDchDCM.h \
  mNewDchFEMModule.hh \
  libdch_LinkDef.h

include_HEADERS = \
  DchSagCorrector.h \
  DchAnaPar.h \
  DchArray.h \
  DchCandidate.hh \
  DchEventInfo.hh \
  DchFastCandidateFinder.hh \
  DchHitAssociator.hh \
  DchHitInfo.hh \
  DchRawTable.hh \
  DchHitLineTable.hh \
  DchHitLineTablev1.hh \
  DchHitLineTablev2.hh \
  DchRawTablev1.hh \
  DchHitLineOut.hh \
  DchHitLineOutv1.hh \
  DchHitLineOutv2.hh \
  DchRawOut.hh \
  DchRawOutv1.hh \
  DchHitLine.hh \
  DchHitLineLists.hh \
  DchLineTrackFit.hh \
  DchLineTracker.hh \
  DchMcRecoTrack.hh \
  DchPc1Hit.hh \
  DchPc1HitLists.hh\
  DchPutDCM.h \
  DchRawInfo.hh \
  DchSimPar.h \
  DchSubs_DCM.h \
  DchTrackCandidate.hh \
  DchTrackInfo.hh \
  PHDchHistogrammer.hh \
  dDchDCM.h \
  dDchDCMPar.h \
  dDchDCMParWrapper.h \
  dDchDCMWrapper.h \
  dDchEvalPar.h \
  dDchEvalParWrapper.h \
  dDchFEM.h \
  dDchFEMWrapper.h \
  dDchFastSimPar.h \
  dDchFastSimParWrapper.h \
  dDchGhitHits.h \
  dDchGhitHitsWrapper.h \
  dDchGhitRaw.h \
  dDchGhitRawWrapper.h \
  dDchHit.h \
  dDchHitWrapper.h \
  dDchNibbleGhit.h \
  dDchNibbleGhitWrapper.h \
  dDchPerfPar.h \
  dDchPerfParWrapper.h \
  dDchRaw.h \
  dDchRawHits.h \
  dDchRawHitsWrapper.h \
  dDchRawWrapper.h \
  dDchReconstructionPar.h \
  dDchReconstructionParWrapper.h \
  dDchTracks.h \
  dDchTracksWrapper.h \
  dDchTracksExt.h\
  dDchTracksExtWrapper.h\
  dDchUnpackPar.h \
  dDchUnpackParWrapper.h \
  mDchDCMModule.h \
  mDchVertexFit.hh \
  mNewDchAnalyzer.hh \
  mNewDchBuilder.hh \
  mNewDchCalibrator.hh \
  mNewDchCandidatory.hh \
  mNewDchDriftVelocityCalibrator.hh \
  mNewDchFastSimulator.hh \
  mNewDchSimulator.hh \
  mNewDchFEMModule.hh \
  mNewDchInitializer.hh \
  mNewDchNoFieldCandidatory.hh \
  mNewDchNoiseAnalyzer.hh \
  mNewDchPerfectTracker.hh \
  mNewDchEfficiencyCalibrator.hh \
  mNewDchUnpacker.hh \
  DchTrack.h \
  DchTrackv1.h \
  DchSnglTrackv1.h

libdch_la_SOURCES = \
  DchArray.cc \
  DchCandidate.C \
  DchSagCorrector.C \
  DchFastCandidateFinder.C \
  DchGetDCM.cc \
  DchHitAssociator.C \
  DchHitInfo.C \
  DchRawTable.C \
  DchHitLineTable.C \
  DchHitLineTablev1.C \
  DchHitLineTablev2.C \
  DchRawTablev1.C \
  DchHitLineOut.C \
  DchHitLineOutv1.C \
  DchHitLineOutv2.C \
  DchRawOut.C \
  DchRawOutv1.C \
  DchHitLine.C \
  DchHitLineLists.C \
  DchLineTrackFit.C \
  DchLineTracker.C \
  DchMcRecoTrack.C \
  DchPc1Hit.C \
  DchPc1HitLists.C \
  DchPutDCM.cc \
  DchRawInfo.C \
  DchSimPar.cc \
  DchTrackCandidate.C \
  DchTrackInfo.C \
  PHDchHistogrammer.C \
  dDchDCMParWrapper.C \
  dDchDCMWrapper.C \
  dDchEvalParWrapper.C \
  dDchFEMWrapper.C \
  dDchFastSimParWrapper.C \
  dDchGhitHitsWrapper.C \
  dDchGhitRawWrapper.C \
  dDchHitWrapper.C \
  dDchNibbleGhitWrapper.C \
  dDchPerfParWrapper.C \
  dDchRawHitsWrapper.C \
  dDchRawWrapper.C \
  dDchReconstructionParWrapper.C \
  dDchTracksWrapper.C \
  dDchTracksExtWrapper.C\
  dDchUnpackParWrapper.C \
  dcharray_tpl.cc \
  mDchDCM.cc \
  mDchDCMModule.C \
  mDchDCMPAM.C \
  mDchVertexFit.C \
  mNewDchAnalyzer.C \
  mNewDchBuilder.C \
  mNewDchCalibrator.C \
  mNewDchCandidatory.C \
  mNewDchDriftVelocityCalibrator.C \
  mNewDchFastSimulator.C \
  mNewDchSimulator.C \
  mNewDchFEMModule.C \
  mNewDchInitializer.C \
  mNewDchNoFieldCandidatory.C \
  mNewDchNoiseAnalyzer.C \
  mNewDchPerfectTracker.C \
  mNewDchEfficiencyCalibrator.C \
  mNewDchUnpacker.C \
  DchTrack.C \
  DchTrackv1.C \
  DchSnglTrackv1.C

nodist_libdch_la_SOURCES = libdch_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libdch.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: \
  dDchDCMParWrapper.h \
  dDchDCMWrapper.h \
  dDchEvalParWrapper.h \
  dDchFEMWrapper.h \
  dDchFastSimParWrapper.h \
  dDchGhitHitsWrapper.h \
  dDchGhitRawWrapper.h \
  dDchHitWrapper.h \
  dDchNibbleGhitWrapper.h \
  dDchPerfParWrapper.h \
  dDchRawHitsWrapper.h \
  dDchRawWrapper.h \
  dDchReconstructionParWrapper.h \
  dDchTracksExtWrapper.h \
  dDchTracksWrapper.h \
  dDchUnpackParWrapper.h \
  DchHitLineOut.hh \
  DchHitLineOutv1.hh \
  DchHitLineOutv2.hh \
  DchHitLineTable.hh \
  DchHitLineTablev1.hh \
  DchHitLineTablev2.hh \
  DchRawOut.hh \
  DchRawOutv1.hh \
  DchRawTable.hh \
  DchRawTablev1.hh \
  DchSnglTrackv1.h \
  DchTrack.h \
  DchTrackv1.h \
  libdch_LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
