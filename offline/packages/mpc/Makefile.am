AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

ACLOCAL_AMFLAGS = -I m4
AM_CXXFLAGS = -Wall -Werror -Wextra -Wno-unused-parameter

INCLUDES = \
  -isystem $(includedir) \
  -isystem $(OFFLINE_MAIN)/include \
  -isystem $(ROOTSYS)/include 

RINCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libmpc.la   


AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(ROOTSYS)/lib

libmpc_la_LIBADD = \
  -lpdbcalBase \
  -lnanoDST \
  -lphflag \
  -lphool \
  -lphtime \
  -lPISARoot \
  -lSubsysReco \
  -ltrigger \
  -lvtx \
  -lgea \
  -lbbc \
  -lPHPythia \
  -lPHHijing \
  -lMinuit

noinst_HEADERS = \
  mpcLinkDef.h

include_HEADERS = \
  mpchalf.h \
  mpcSample.h \
  mpcSampleV1.h \
  mpcSampleContainer.h \
  mpcSampleContainerV1.h \
  mpcRawContent.h \
  mpcRawContentV1.h \
  mpcRawContentV2.h \
  mpcRawContentV3.h \
  mpcRawContentV4.h \
  mpcRawContentV5.h \
  mpcRawContainer.h \
  mpcRawContainerV1.h \
  mpcRawContainerV2.h \
  mpcRawContainerV3.h \
  mpcRawContainerV4.h \
  mpcRawContainerV5.h \
  mpcTowerContent.h \
  mpcTowerContentV1.h \
  mpcSimTowerContentV1.h \
  mpcTowerContainer.h \
  mpcTowerContainerV1.h \
  mpcSimTowerContainerV1.h \
  mpcClusterContent.h \
  mpcClusterContentV1.h \
  mpcClusterContentV2.h \
  mpcClusterContainer.h \
  mpcClusterContainerV1.h \
  mpcClusterContainerV2.h \
  mpcGeaTowerContent.h \
  mpcGeaTowerContentV1.h \
  mpcGeaTowerContainer.h \
  mpcGeaTowerContainerV1.h \
  mpcGeaClusterContent.h \
  mpcGeaClusterContentV1.h \
  mpcGeaClusterContainer.h \
  mpcGeaClusterContainerV1.h \
  mpcNoiseContainer.h \
  mpc2x2Content.h \
  mpc2x2ContentV1.h \
  mpc4x4Content.h \
  mpc4x4ContentV1.h \
  mpc2x2Container.h \
  mpc2x2ContainerV1.h \
  mpc4x4Container.h \
  mpc4x4ContainerV1.h \
  MpcOut.h \
  MpcOutV1.h \
  MpcSectorRec.h \
  MpcSectorRecV2.h \
  MpcCluster.h \
  MpcEvent.h \
  MpcCalib.h \
  MpcMap.h \
  MpcTriggerMapping.h \
  MpcNoise.h \
  MpcScale.h \
  MpcWarnMap.h \
  MpcEmcTrigEmulator.h \
  MpcTrigEmulator.h \
  MpcRandom.h \
  MpcBackCorrReco.h \
  MpcClusterIDReco.h \
  MpcEmbedReco.h \
  MpcGainCorrReco.h \
  MpcGeaClusterReco.h \
  MpcMakeDeadReco.h \
  MpcRecalReco.h \
  MpcTrigReco.h \
  MpcReco.h

libmpc_la_SOURCES = \
  mpchalf.c \
  mpcSampleContainer.C \
  mpcSampleContainerV1.C \
  mpcSample.C \
  mpcSampleV1.C \
  mpcRawContainer.C \
  mpcRawContainerV1.C \
  mpcRawContainerV2.C \
  mpcRawContainerV3.C \
  mpcRawContainerV4.C \
  mpcRawContainerV5.C \
  mpcRawContent.C \
  mpcRawContentV1.C \
  mpcRawContentV2.C \
  mpcRawContentV3.C \
  mpcRawContentV4.C \
  mpcRawContentV5.C \
  mpcTowerContainer.C \
  mpcTowerContainerV1.C \
  mpcSimTowerContainerV1.C \
  mpcTowerContent.C \
  mpcTowerContentV1.C \
  mpcSimTowerContentV1.C \
  mpcClusterContent.C \
  mpcClusterContentV1.C \
  mpcClusterContentV2.C \
  mpcClusterContainer.C \
  mpcClusterContainerV1.C \
  mpcClusterContainerV2.C \
  mpcGeaTowerContainer.C \
  mpcGeaTowerContainerV1.C \
  mpcGeaTowerContent.C \
  mpcGeaTowerContentV1.C \
  mpcGeaClusterContainer.C \
  mpcGeaClusterContainerV1.C \
  mpcGeaClusterContent.C \
  mpcGeaClusterContentV1.C \
  mpcNoiseContainer.C \
  mpc2x2Content.C \
  mpc2x2ContentV1.C \
  mpc4x4Content.C \
  mpc4x4ContentV1.C \
  mpc2x2Container.C \
  mpc2x2ContainerV1.C \
  mpc4x4Container.C \
  mpc4x4ContainerV1.C \
  MpcRandom.C \
  MpcOut.C \
  MpcOutV1.C \
  MpcSectorRec.C \
  MpcSectorRecV2.C \
  MpcCluster.C \
  MpcEvent.C \
  MpcMap.C \
  MpcTriggerMapping.C \
  MpcCalib.C \
  MpcTrigReco.C \
  MpcReco.C \
  MpcBackCorrReco.C \
  MpcGainCorrReco.C \
  MpcEmbedReco.C \
  MpcRecalReco.C \
  MpcGeaClusterReco.C \
  MpcClusterIDReco.C \
  MpcMakeDeadReco.C \
  MpcNoise.C \
  MpcEmcTrigEmulator.C \
  MpcTrigEmulator.C \
  MpcWarnMap.C \
  MpcScale.C

nodist_libmpc_la_SOURCES = mpc_Dict.C

mpc_Dict.C: mpcSampleContainer.h mpcSample.h \
            mpcSampleContainerV1.h mpcSampleV1.h \
            mpcRawContainer.h mpcRawContent.h \
            mpcRawContainerV1.h mpcRawContentV1.h \
            mpcRawContainerV2.h mpcRawContentV2.h \
            mpcRawContainerV3.h mpcRawContentV3.h \
            mpcRawContainerV4.h mpcRawContentV4.h \
	    mpcRawContainerV5.h mpcRawContentV5.h \
            mpcTowerContainer.h mpcTowerContainerV1.h \
            mpcTowerContent.h mpcTowerContentV1.h \
            mpcSimTowerContentV1.h mpcSimTowerContainerV1.h \
            mpcClusterContainer.h \
            mpcClusterContainerV1.h mpcClusterContentV1.h \
            mpcClusterContainerV2.h mpcClusterContentV2.h \
            mpcGeaTowerContainer.h mpcGeaTowerContainerV1.h \
            mpcGeaTowerContent.h mpcGeaTowerContentV1.h \
            mpcGeaClusterContainer.h mpcGeaClusterContainerV1.h \
            mpcGeaClusterContent.h mpcGeaClusterContentV1.h \
	    mpcNoiseContainer.h \
            mpc2x2Content.h mpc2x2ContentV1.h \
            mpc4x4Content.h mpc4x4ContentV1.h \
            mpc2x2Container.h mpc2x2ContainerV1.h \
            mpc4x4Container.h mpc4x4ContainerV1.h \
            MpcOut.h MpcOutV1.h \
            MpcMap.h MpcCalib.h MpcNoise.h MpcWarnMap.h \
            MpcTriggerMapping.h \
            MpcReco.h MpcRecalReco.h MpcBackCorrReco.h \
            MpcGainCorrReco.h MpcEmbedReco.h \
            MpcGeaClusterReco.h MpcClusterIDReco.h MpcMakeDeadReco.h \
            MpcScale.h MpcEmcTrigEmulator.h MpcTrigEmulator.h \
            MpcTrigReco.h \
            mpcLinkDef.h
	rootcint -f $@ -c $(RINCLUDES) $^

noinst_PROGRAMS = testexternals

testexternals_SOURCES = testexternals.C

testexternals_LDADD = libmpc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict*

