AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

# This is the preprocessor macro that gets set when -pthread is used with 
# gcc. I can't remember the issue involved with using -pthread, so I try 
# this instead. (and the implicit addition of -lpthread from configure.in)
AM_CPPFLAGS = -D_REENTRANT

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib 

lib_LTLIBRARIES = \
  liblvl2.la

liblvl2_la_LIBADD = \
  -lNoRootEvent \
  -lSubsysReco \
  -lheader \
  -llvl2dstobjects \
  -lphflag \
  -lphool \
  -ltrigger \
  -lboost_date_time \
  -lboost_thread

noinst_HEADERS = \
  Lvl2OutLinkDef.h \
  L2PC1PC3TrackEMCAssocDSTLinkDef.h \
  L2SinglePC1PC3TrackEMCAssocMicroV1LinkDef.h \
  L2PC1PC3TrackEMCAssocMicroV1LinkDef.h \
  L2ElecCanddtLowOcupyDSTLinkDef.h \
  L2ElecInvMassLowOcupyDSTLinkDef.h \
  L2ElecCanddtLowOcupyMicrov1LinkDef.h \
  L2ElecInvMassLowOcupyMicroV1LinkDef.h \
  L2ElecSnglCanddtLowOcupyMicrov1LinkDef.h \
  L2ElecSnglInvMassLowOcupyMicroV1LinkDef.h \
  L2TrackEMCLowOcupyDSTLinkDef.h \
  L2TrackPC3LowOcupyDSTLinkDef.h \
  L2TrackEMCLowOcupyMicroV1LinkDef.h \
  L2TrackPC3LowOcupyMicroV1LinkDef.h \
  L2SingleTrackEMCLowOcupyMicroV1LinkDef.h \
  L2SingleTrackPC3LowOcupyMicroV1LinkDef.h \
  L2TofHitMicroV1LinkDef.h \
  L2TofTwoHitTriggerDSTLinkDef.h \
  L2TofTwoHitTriggerMicroV1LinkDef.h \
  L2ElectronCandidateMicroLinkDef.h \
  L2JPsieeInvMassMicroLinkDef.h \
  L2PHIeeInvMassMicroLinkDef.h \
  L2ElectronCandidateDSTLinkDef.h \
  L2JPsieeInvMassDSTLinkDef.h \
  L2PHIeeInvMassDSTLinkDef.h 

include_HEADERS = \
  Lvl2Struct.h \
  Lvl1Struct.h \
  Lvl2ConfigAccessor.h \
  Lvl2Out.h \
  Lvl2Outv1.h \
  Lvl2SnglOut.h \
  Lvl2SnglOutv1.h \
  Lvl2SnglOutv2.h \
  Lvl2OutArray.h \
  Lvl2OutArrayv1.h \
  Lvl2OutArrayv2.h \
  L2PC1PC3TrackEMCAssocDST.h \
  L2SinglePC1PC3TrackEMCAssocMicroV1.h \
  L2ElecSnglInvMassLowOcupyMicroV1.h \
  L2ElecSnglCanddtLowOcupyMicrov1.h \
  L2ElecInvMassLowOcupyMicroV1.h\
  L2ElecCanddtLowOcupyMicrov1.h\
  L2ElecCanddtLowOcupyDST.h\
  L2ElecInvMassLowOcupyDST.h\
  L2TrackEMCLowOcupyDST.h \
  L2TrackPC3LowOcupyDST.h \
  L2TrackEMCLowOcupyMicroV1.h \
  L2TrackPC3LowOcupyMicroV1.h \
  L2SingleTrackEMCLowOcupyMicroV1.h \
  L2SingleTrackPC3LowOcupyMicroV1.h \
  L2TofHitMicroV1.h \
  L2TofTwoHitTriggerDST.h \
  L2TofTwoHitTriggerMicroV1.h \
  Run2Lvl2AlgorithmIndexList.h \
  L2ElectronCandidateMicrov1.h \
  L2ElectronSnglCandidateMicrov1.h \
  L2JPsieeInvMassMicrov1.h \
  L2JPsieeInvSnglMassMicrov1.h \
  L2PHIeeInvMassMicrov1.h \
  L2PHIeeInvSnglMassMicrov1.h \
  L2ElectronCandidateDST.h \
  L2JPsieeInvMassDST.h \
  L2PHIeeInvMassDST.h 

liblvl2_la_SOURCES = \
  Lvl2OutDict.C \
  Lvl2Out.cc \
  Lvl2Outv1.cc \
  Lvl2SnglOut.cc \
  Lvl2SnglOutv1.cc \
  Lvl2SnglOutv2.cc \
  Lvl2OutArray.cc \
  Lvl2OutArrayv1.cc \
  Lvl2OutArrayv2.cc \
  L2PC1PC3TrackEMCAssocDST.C \
  L2PC1PC3TrackEMCAssocDSTDict.C \
  L2SinglePC1PC3TrackEMCAssocMicroV1.C \
  L2SinglePC1PC3TrackEMCAssocMicroV1Dict.C \
  L2ElecSnglInvMassLowOcupyMicroV1.C \
  L2ElecSnglInvMassLowOcupyMicroV1Dict.C \
  L2ElecSnglCanddtLowOcupyMicrov1.C \
  L2ElecSnglCanddtLowOcupyMicrov1Dict.C \
  L2ElecInvMassLowOcupyMicroV1.C \
  L2ElecInvMassLowOcupyMicroV1Dict.C \
  L2ElecCanddtLowOcupyMicrov1.C \
  L2ElecCanddtLowOcupyMicrov1Dict.C \
  L2ElecCanddtLowOcupyDST.C \
  L2ElecCanddtLowOcupyDSTDict.C \
  L2ElecInvMassLowOcupyDST.C \
  L2ElecInvMassLowOcupyDSTDict.C \
  L2TrackEMCLowOcupyDST.C \
  L2TrackEMCLowOcupyDSTDict.C \
  L2TrackPC3LowOcupyDST.C \
  L2TrackPC3LowOcupyDSTDict.C \
  L2TrackEMCLowOcupyMicroV1.C \
  L2TrackEMCLowOcupyMicroV1Dict.C \
  L2TrackPC3LowOcupyMicroV1.C \
  L2TrackPC3LowOcupyMicroV1Dict.C \
  L2SingleTrackEMCLowOcupyMicroV1.C \
  L2SingleTrackEMCLowOcupyMicroV1Dict.C \
  L2SingleTrackPC3LowOcupyMicroV1.C \
  L2SingleTrackPC3LowOcupyMicroV1Dict.C \
  L2TofHitMicroV1.C \
  L2TofHitMicroV1Dict.C \
  L2TofTwoHitTriggerDST.C \
  L2TofTwoHitTriggerDSTDict.C \
  L2TofTwoHitTriggerMicroV1.C \
  L2TofTwoHitTriggerMicroV1Dict.C \
  Lvl2PrimitivesCheck.C   \
  L2ElectronCandidateMicrov1.C \
  L2ElectronSnglCandidateMicrov1.C \
  L2ElectronCandidateMicro_Dict.C \
  L2PHIeeInvMassMicrov1.C \
  L2PHIeeInvSnglMassMicrov1.C \
  L2PHIeeInvSnglMassMicro_Dict.C \
  L2JPsieeInvMassMicrov1.C \
  L2JPsieeInvSnglMassMicrov1.C \
  L2JPsieeInvSnglMassMicro_Dict.C \
  L2ElectronCandidateDST.C \
  L2ElectronCandidateDSTDict.C \
  L2JPsieeInvMassDST.C \
  L2JPsieeInvMassDSTDict.C \
  L2PHIeeInvMassDST.C \
  L2PHIeeInvMassDSTDict.C 

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  liblvl2.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries for tables.

%Dict.C: %.h %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

Lvl2OutDict.C : Lvl2Out.h Lvl2Outv1.h Lvl2OutArray.h Lvl2OutArrayv1.h Lvl2OutArrayv2.h Lvl2SnglOut.h Lvl2SnglOutv1.h Lvl2SnglOutv2.h Lvl2OutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


Lvl2RecoModulesDict.C : \
  Lvl2StatsEval.h \
  Lvl2PrimitivesCheck.h \
  Lvl2PrimitivesCompare.h \
  Lvl2DecisionCompare.h \
  Lvl2FilterMon.h \
  Lvl2RunTrigSelect.h \
  Lvl2RecoModulesLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


L2ElectronCandidateMicro_Dict.C: L2ElectronCandidateMicrov1.h L2ElectronSnglCandidateMicrov1.h L2ElectronCandidateMicroLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

L2PHIeeInvSnglMassMicro_Dict.C: L2PHIeeInvMassMicrov1.h L2PHIeeInvSnglMassMicrov1.h L2PHIeeInvMassMicroLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

L2JPsieeInvSnglMassMicro_Dict.C: L2JPsieeInvMassMicrov1.h L2JPsieeInvSnglMassMicrov1.h L2JPsieeInvMassMicroLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict* ${BUILT_SOURCES}

testexternals_SOURCES = testexternals.C
