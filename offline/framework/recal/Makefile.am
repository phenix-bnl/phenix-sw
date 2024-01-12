#
# Please order according to the Roman alphabet - it is a nightmare to find
# anything in this mess
# Please DO NOT USE TABS instead of spaces as indentation. 
# They have special meanings (the actual command follows them). If you have
# a typo, e.g. forgotten continuation line, they are interpreted as command
# leading to really hard to interprete error messages
#
AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -isystem $(OFFLINE_MAIN)/include/eigen3 \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include

RINCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(OPT_PHENIX)/include

include_HEADERS = \
  DeathToMemoryHogs.h \
  EmcGenericDeadRecalReco.h \
  MasterRecalibrator.h \
  MasterRecalibratorManager.h \
  Recalibrator.h \
  TCentralityRecalibratorBBC.h \
  SvxCentralTrackRecalList.h \
  SvxCentralTrackRecal.h \
  SvxMomentumRecal.h

lib_LTLIBRARIES = \
  librecal.la

noinst_HEADERS =

# Please order according to the Roman alphabet - it is a nightmare to find
# anything in this mess
librecal_la_SOURCES = \
  AccrecalReco.C \
  Bbcrecal.C \
  BunchCross.C \
  DeathToMemoryHogs.C \
  DepRecalibrator.C \
  EmcGenericDeadRecalReco.C \
  EmcGenericEScaleRecalReco.C \
  EmcPidrecalReco.C \
  EmctofrecalReco.C \
  EmcTofWalkRecalReco.C \
  EmcTrkMatchingRecalReco.C \
  EmcElMatchingRecalReco.C \
  HbdTrkMatchingRecalReco.C \
  HbdRecalReco.C \
  MasterRecalibrator.C \
  MasterRecalibratorManager.C \
  MatchrecalReco.C \
  MatchrecalRecoRun5.C \
  MatchrecalRecoRun6.C \
  MatchrecalRecoRun7.C \
  Matchrecal_dAu_Reco.C \
  MatchrecalReco39GeVRun10.C \
  MatchrecalReco62GeVRun10.C \
  MatchrecalReco7GeVRun10.C \
  MatchrecalReco19GeVRun11.C \
  MatchrecalReco27GeVRun11.C \
  MatchrecalRecoRun12UU192.C \
  MatchrecalRecoRun14AuAu15GeV.C \
  MatchrecalRecoRun14HeAu.C \
  MatchrecalRecoRun15pAu.C \
  MatchrecalRecoRun15pAl.C \
  MatchrecalRecoRun15pp.C \
  MatchrecalReco200GeVRun16dAu.C \
  MatchrecalReco62GeVRun16dAu.C \
  MatchrecalReco39GeVRun16dAu.C \
  MatchrecalReco20GeVRun16dAu.C \
  MatchrecalRecoRun16AuAu200GeV.C \
  MomChangeRecalReco.C \	
  MomChangeRecal_dAu_Reco.C \
  NoiseEvents.C \
  PercentileRecalReco.C \
  PhCglListRecalReco.C \
  PidrecalReco.C \
  Pidrecal_dAu_Reco.C \
  ReactionPlaneReco.C \
  ReactionPlaneRecalReco.C \
  Run8ReactionPlaneReco.C \
  RpParManager.C \
  ReadbackCompactCNT.C \
  ReadbackCompactPWG.C \
  Recalibrator.C \
  Recalibrator_Dict.C \
  Run4ReactionPlaneRecalReco.C \
  Run4ReactionPlaneReco.C \
  Run5CuCu200GeVCentralityReco.C \
  Run5CuCu62GeVCentralityReco.C \
  Run5PPEmctofRecalReco.C \
  Run56PPEmcphotontofRecalReco.C \
  Run8PC3MatchRecal.C \
  Run12pp200PC3MatchRecal.C \
  Run12pp200EMCMatchRecal.C \
  Run7AuAu200GeVCentralityReco.C \
  Run10AuAu200GeVCentralityReco.C \
  Run8dAu200GeVCentralityReco.C \
  Run10AuAu62GeVCentralityReco.C \
  Run10AuAu39GeVCentralityReco.C \
  Run10AuAu007GeVCentralityReco.C \
  Run11AuAu200ElectronEMCMatching.C \
  Run11AuAu200GeVCentralityReco.C \
  Run11AuAu027GeVCentralityReco.C \
  Run11AuAu019GeVCentralityReco.C \
  Run11AuAu200GeVDepRecal.C \
  Run14AuAu200DepRecal.C \
  Run15pp200DepRecal.C \
  Run15ppEMCMatching.C \
  Run14AuAu015GeVCentralityReco.C \
  Run14AuAu200GeVCentralityReco.C \
  Run14AuAu200ElectronEMCMatching.C \
  Run14AuAu200PC3MatchRecal.C \
  Run14AuAu200PC2MatchRecal.C \
  Run14AuAu200EMCMatchRecal.C \
  Run15pp200ElectronEMCMatching.C \
  Run14He3Au200GeVCentralityReco.C \
  Run15pAu200GeVCentralityReco.C \
  Run15pAuEMCMatching.C \
  Run15pAl200GeVCentralityReco.C \
  Run16dAu200GeVCentralityReco.C \
  Run16dAu62GeVCentralityReco.C \
  Run16dAu20GeVCentralityReco.C \
  Run16dAu39GeVCentralityReco.C \
  Run16AuAu200GeVCentralityReco.C \
  Run12CuAu200GeVCentralityReco.C \
  Run12CuAu200GeVTOF_PC3_Matching.C \
  Run12UU193GeVCentralityReco.C \
  SvxCentralTrackRecalList.C \
  SvxCentralTrackRecal.C \
  SvxCentralTrackRecalReco.C \
  SvxCentralTrackReFit.C \
  SvxCentralTrackFitRecal.C \
  SvxMomentumRecal.C \
  SvxReactionPlaneMergeReco.C \
  SvxReactionPlaneReco.C \
  SvxReactionPlaneRecalReco.C \
  TCentralityRecalibratorBBC.C \
  TofrecalReco.C \
  TofwMatchRecalRun10.C\
  TofwrecalReco.C \
  TecChargeRecal.C \
  TofwMatchRecal.C \
  TRDLikeReco.C \
  VtxPixelRealignRecal.C\
  Run15pAu200ElectronEMCMatching.C\
  Run15pAu200DepRecal.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

librecal_la_LIBADD = \
  -lCNT \
  -lnanoDST \
  -lHardpDST \
  -lpdbcalBase \
  -lphool \
  -lreactionplane \
  -lSubsysReco \
  -lsvx \
  -ltec \
  -ltecpid \
  -ltof \
  -lhbd \
  -ltrigger \
  -lcompactCNT \
  -lemc

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  librecal.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Please order according to the Roman alphabet - it is a nightmare to find
# anything in this mess
Recalibrator_Dict.C: \
  AccrecalReco.h \
  Bbcrecal.h \
  BunchCross.h \
  DeathToMemoryHogs.h \
  DepRecalibrator.h \
  EmcGenericDeadRecalReco.h \
  EmcGenericEScaleRecalReco.h \
  EmcPidrecalReco.h \
  EmctofrecalReco.h \
  EmcTofWalkRecalReco.h \
  EmcTrkMatchingRecalReco.h \
  EmcElMatchingRecalReco.h \
  HbdTrkMatchingRecalReco.h \
  HbdRecalReco.h \
  MasterRecalibrator.h \
  MasterRecalibratorManager.h \
  MatchrecalReco.h \
  MatchrecalRecoRun5.h \
  MatchrecalRecoRun6.h \
  MatchrecalRecoRun7.h \
  Matchrecal_dAu_Reco.h \
  MatchrecalReco39GeVRun10.h \
  MatchrecalReco62GeVRun10.h \
  MatchrecalReco7GeVRun10.h \
  MatchrecalReco19GeVRun11.h \
  MatchrecalReco27GeVRun11.h \
  MatchrecalRecoRun12UU192.h \
  MatchrecalRecoRun14AuAu15GeV.h \
  MatchrecalRecoRun14HeAu.h \
  MatchrecalRecoRun15pAu.h \
  MatchrecalRecoRun15pAl.h \
  MatchrecalRecoRun15pp.h \
  MatchrecalRecoRun16AuAu200GeV.h \
  MatchrecalReco200GeVRun16dAu.h \
  MatchrecalReco62GeVRun16dAu.h \
  MatchrecalReco39GeVRun16dAu.h \
  MatchrecalReco20GeVRun16dAu.h \
  MomChangeRecalReco.h \
  MomChangeRecal_dAu_Reco.h \
  NoiseEvents.h \
  PercentileRecalReco.h \
  PhCglListRecalReco.h \
  PidrecalReco.h \
  Pidrecal_dAu_Reco.h \
  ReactionPlaneReco.h \
  Run8ReactionPlaneReco.h \
  ReactionPlaneRecalReco.h \
  RpParManager.h \
  ReadbackCompactCNT.h \
  ReadbackCompactPWG.h \
  Recalibrator.h \
  Run4ReactionPlaneRecalReco.h \
  Run4ReactionPlaneReco.h \
  Run5CuCu200GeVCentralityReco.h \
  Run5CuCu62GeVCentralityReco.h \
  Run5PPEmctofRecalReco.h \
  Run56PPEmcphotontofRecalReco.h \
  Run8PC3MatchRecal.h \
  Run12pp200PC3MatchRecal.h \
  Run12pp200EMCMatchRecal.h \
  Run10AuAu200GeVCentralityReco.h \
  Run7AuAu200GeVCentralityReco.h \
  Run8dAu200GeVCentralityReco.h \
  Run10AuAu62GeVCentralityReco.h \
  Run10AuAu39GeVCentralityReco.h \
  Run10AuAu007GeVCentralityReco.h \
  Run11AuAu200ElectronEMCMatching.h \
  Run11AuAu200GeVDepRecal.h \
  Run11AuAu200GeVCentralityReco.h \
  Run11AuAu027GeVCentralityReco.h \
  Run11AuAu019GeVCentralityReco.h \
  Run12CuAu200GeVCentralityReco.h \
  Run12CuAu200GeVTOF_PC3_Matching.h \
  Run12UU193GeVCentralityReco.h \
  Run14AuAu015GeVCentralityReco.h \
  Run14AuAu200GeVCentralityReco.h \
  Run14AuAu200ElectronEMCMatching.h \
  Run14AuAu200PC3MatchRecal.h \
  Run14AuAu200PC2MatchRecal.h \
  Run14AuAu200EMCMatchRecal.h \
  Run15pp200ElectronEMCMatching.h \
  Run15pp200DepRecal.h \
  Run15ppEMCMatching.h \
  Run14AuAu200DepRecal.h \
  Run14He3Au200GeVCentralityReco.h \
  Run15pAu200GeVCentralityReco.h \
  Run15pAuEMCMatching.h \
  Run15pAl200GeVCentralityReco.h \
  Run16dAu200GeVCentralityReco.h \
  Run16dAu62GeVCentralityReco.h \
  Run16dAu20GeVCentralityReco.h \
  Run16dAu39GeVCentralityReco.h \
  Run16AuAu200GeVCentralityReco.h \
  SvxReactionPlaneMergeReco.h \
  SvxReactionPlaneReco.h \
  SvxReactionPlaneRecalReco.h \
  SvxCentralTrackRecalList.h \
  SvxCentralTrackRecal.h \
  SvxCentralTrackRecalReco.h \
  SvxCentralTrackReFit.h \
  SvxCentralTrackFitRecal.h \
  SvxMomentumRecal.h \
  TCentralityRecalibratorBBC.h \
  TecChargeRecal.h \
  TofrecalReco.h \
  TofwMatchRecalRun10.h \
  TofwrecalReco.h \
  TRDLikeReco.h \
  TofwMatchRecal.h \
  VtxPixelRealignRecal.h \
  Run15pAu200ElectronEMCMatching.h \
  Run15pAu200DepRecal.h \
  RecalibratorLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(RINCLUDES) $^

clean-local:
	rm -f *Dict*
