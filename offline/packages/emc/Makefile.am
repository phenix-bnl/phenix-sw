AUTOMAKE_OPTIONS = foreign

AM_CFLAGS = -Wall -Werror -Wno-error=unused-but-set-variable
AM_CXXFLAGS = ${AM_CFLAGS}

lib_LTLIBRARIES = \
  libemc-common.la libemc-datatypes.la \
  libemc-calibration.la libemc-reconstruction.la \
  libemc-simulation.la libemc-simulation2.la libemc-realdata.la \
  libemc.la

BUILT_SOURCES =
CLEAN_FILES = 
noinst_HEADERS =
noinst_PROGRAMS =
include_HEADERS =



INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib 

BUILT_SOURCES += testexternals.C

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@








##############################################################################
###
###  common code for emc-* libs
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-common
testexternals_common_SOURCES = testexternals.C
testexternals_common_LDADD = libemc-common.la



# -lPISARoot should be added?
libemc_common_la_LIBADD = \
  -lSubsysReco \
  -lfun4all \
  -lphool -lphgeo \
  -lEmcStatic -lemcDataManager -lTOAD

noinst_HEADERS += \
  mEmcGeometryModuleLinkDef.h

# emcDBMS probably should not be exported..
include_HEADERS += \
  emcDBMS.h \
  emcNodeHelper.h \
  mEmcGeometryModule.h

libemc_common_la_SOURCES = \
  emcDBMS.C emcDBMSDict.C \
  emcNodeHelper.C \
  mEmcGeometryModule.C mEmcGeometryDict.C

emcDBMSDict.C: emcDBMS.h emcDBMSLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^




# obsolete stuff

#noinst_HEADERS += 

include_HEADERS += \
  emcObjectFiller.h \
  emcObjectFillerManager.h

libemc_common_la_SOURCES += \
  emcObjectFiller.C \
  emcObjectFillerManager.C \
  emcObjectFillerRegistry.C \
  emcObjectFillerDict.C
#  emcOFTowerContainer.C \
#  emcOFClusterContainer.C

emcObjectFillerDict.C: emcObjectFiller.h emcObjectFillerManager.h emcObjectFillerRegistry.h emcObjectFillerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

#emcDSTHistogrammerDict.C: emcDSTHistogrammer.h emcDSTHistogrammerLinkDef.h
#	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^







##############################################################################
###
###  various datatypes
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-datatypes
testexternals_datatypes_SOURCES = testexternals.C
testexternals_datatypes_LDADD = libemc-datatypes.la



libemc_datatypes_la_LIBADD = \
  libemc-common.la \
  -lphool -lSubsysReco \
  -lEmcStatic -lemcCalib2 \
  -lWrappers

#noinst_HEADERS +=

include_HEADERS += \
  emcClusterContainer.h emcClusterContainerT.h emcClusterContent.h \
  emcClusterContainerv1.h emcClusterContentv1.h \
  emcClusterContainerv1S.h emcClusterContentv1S.h \
  emcClusterContainerv1M.h emcClusterContentv1M.h \
  emcClusterContainerv2.h emcClusterContentv2.h \
  emcClusterContainerv3.h emcClusterContentv3.h \
  emcClusterContainerv4.h emcClusterContentv4.h \
  emcClusterContainerv5.h emcClusterContentv5.h \
  emcClusterContainerv6.h emcClusterContentv6.h

libemc_datatypes_la_SOURCES = \
  emcClusterContainer.C emcClusterContent.C \
  emcClusterContainerv1.C emcClusterContentv1.C \
  emcClusterContainerv1S.C emcClusterContentv1S.C \
  emcClusterContainerv1M.C emcClusterContentv1M.C \
  emcClusterContainerv2.C emcClusterContentv2.C \
  emcClusterContainerv3.C emcClusterContentv3.C \
  emcClusterContainerv4.C emcClusterContentv4.C \
  emcClusterContainerv5.C emcClusterContentv5.C \
  emcClusterContainerv6.C emcClusterContentv6.C \
  emcClusterDict.C

emcClusterDict.C: emcClusterContainer.h emcClusterContent.h \
	emcClusterContainerv1.h emcClusterContentv1.h \
	emcClusterContainerv1M.h emcClusterContentv1M.h \
	emcClusterContainerv1S.h emcClusterContentv1S.h \
	emcClusterContainerv2.h emcClusterContentv2.h \
	emcClusterContainerv3.h emcClusterContentv3.h \
	emcClusterContainerv4.h emcClusterContentv4.h \
	emcClusterContainerv5.h emcClusterContentv5.h \
	emcClusterContainerv6.h emcClusterContentv6.h \
	emcClusterLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^



# obsolete stuff

noinst_HEADERS += \
  EmcCalibTowerLinkDef.h \
  EmcSnglCalibTowerv1.h \
  EmcSnglClusterLocalExtv1.h \
  EmcClusterLocalExtLinkDef.h

include_HEADERS += \
  dEmcCalibTower.h dEmcCalibTowerWrapper.h \
  dEmcCluster.h dEmcClusterWrapper.h \
  dEmcDCMLongData.h dEmcDCMLongDataWrapper.h \
  dEmcDCMData.h dEmcDCMDataWrapper.h \
  dEmcFEMData.h dEmcFEMDataWrapper.h \
  dEmcEvent.h dEmcEventWrapper.h \
  dEmcGeaClusterTrack.h dEmcGeaClusterTrackWrapper.h \
  dEmcGeaHit.h dEmcGeaHitWrapper.h \
  dEmcGeaParams.h dEmcGeaParamsWrapper.h \
  dEmcGeaTowerEval.h dEmcGeaTowerEvalWrapper.h \
  dEmcGeaTowerTrack.h dEmcGeaTowerTrackWrapper.h \
  dEmcGeaTrack.h \
  dEmcGeaTrackCluster.h dEmcGeaTrackClusterWrapper.h \
  dEmcGeaTrackTower.h dEmcGeaTrackTowerWrapper.h \
  dEmcGeaTrackWrapper.h dEmcGeometry.h \
  dEmcGeometryWrapper.h dEmcPerfect.h \
  dEmcPerfectWrapper.h dEmcRawData.h \
  dEmcRawDataWrapper.h dEmcRecoPar.h \
  dEmcRecoParWrapper.h dEmcRespPar.h \
  dEmcRespParWrapper.h \
  dEmcClusterExt.h dEmcClusterExtWrapper.h \
  dEmcClusterLocal.h dEmcClusterLocalWrapper.h \
  dEmcClusterLocalExt.h dEmcClusterLocalExtWrapper.h \
  \
  EmcCalibTower.h EmcCalibTowerv1.h \
  EmcClusterLocalExt.h EmcClusterLocalExtv1.h \
  EmcClusterLocalExtMicrov4.h \
  EmcSnglClusterLocalExtMicrov4.h

libemc_datatypes_la_SOURCES += \
  dEmcCalibTowerWrapper.C dEmcCalibTowerDict.C \
  dEmcClusterWrapper.C dEmcClusterDict.C \
  dEmcDCMLongDataWrapper.C dEmcDCMLongDataDict.C \
  dEmcDCMDataWrapper.C dEmcDCMDataDict.C \
  dEmcFEMDataWrapper.C dEmcFEMDataDict.C \
  dEmcEventWrapper.C dEmcEventDict.C \
  dEmcGeaClusterTrackWrapper.C dEmcGeaClusterTrackDict.C \
  dEmcGeaHitWrapper.C dEmcGeaHitDict.C \
  dEmcGeaParamsWrapper.C dEmcGeaParamsDict.C \
  dEmcGeaTowerEvalWrapper.C dEmcGeaTowerEvalDict.C \
  dEmcGeaTowerTrackWrapper.C dEmcGeaTowerTrackDict.C \
  dEmcGeaTrackClusterWrapper.C dEmcGeaTrackClusterDict.C \
  dEmcGeaTrackTowerWrapper.C dEmcGeaTrackTowerDict.C \
  dEmcGeaTrackWrapper.C dEmcGeaTrackDict.C \
  dEmcGeometryWrapper.C dEmcGeometryDict.C \
  dEmcPerfectWrapper.C dEmcPerfectDict.C \
  dEmcRawDataWrapper.C dEmcRawDataDict.C \
  dEmcRecoParWrapper.C dEmcRecoParDict.C \
  dEmcRespParWrapper.C dEmcRespParDict.C \
  dEmcClusterExtWrapper.C dEmcClusterExtDict.C \
  dEmcClusterLocalExtWrapper.C dEmcClusterLocalExtDict.C \
  dEmcClusterLocalWrapper.C dEmcClusterLocalDict.C \
  \
  EmcCalibTower.C EmcCalibTowerv1.C EmcCalibTower_Dict.C \
  EmcClusterLocalExt.C EmcClusterLocalExtv1.C \
  EmcSnglClusterLocalExtv1.C \
  EmcClusterLocalExtMicrov4.C \
  EmcSnglClusterLocalExtMicrov4.C \
  EmcClusterLocalExt_Dict.C \
  EmcSnglCalibTowerv1.C

EmcClusterLocalExt_Dict.C: EmcClusterLocalExt.h \
                           EmcClusterLocalExtv1.h EmcSnglClusterLocalExtv1.h \
                           EmcClusterLocalExtMicrov4.h EmcSnglClusterLocalExtMicrov4.h \
                           EmcClusterLocalExtLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

EmcCalibTower_Dict.C: EmcCalibTower.h \
                      EmcCalibTowerv1.h EmcSnglCalibTowerv1.h \
                      EmcCalibTowerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^



# new libemc2 datatypes

BUILT_SOURCES += emctypes_dict.cc
CLEAN_FILES += emctypes_dict.h emctypes_dict.cc
include_HEADERS += emctypes.h
noinst_HEADERS += emctypes_dict.h
libemc_datatypes_la_SOURCES += emctypes.cc emctypes_dict.cc



#BUILT_SOURCES += emcContentT_dict.cc emcContainerT_dict.cc
#CLEAN_FILES += emcContentT_dict.h emcContentT_dict.cc emcContainerT_dict.h emcContainerT_dict.cc
include_HEADERS += emcContentT.h emcContainerT.h
#noinst_HEADERS += emcContentT_dict.h emcContainerT_dict.h
#libemc_datatypes_la_SOURCES += emcContentT.cc emcContentT_dict.cc emcContainerT.cc emcContainerT_dict.cc
libemc_datatypes_la_SOURCES += emcContentT.cc emcContainerT.cc 



#BUILT_SOURCES += emcGeaRawData_dict.cc emcGeaRawDataContainer_dict.cc
#CLEAN_FILES += emcGeaRawData_dict.h emcGeaRawData_dict.cc emcGeaRawDataContainer_dict.h emcGeaRawDataContainer_dict.cc
##include_HEADERS += 
#noinst_HEADERS += emcGeaRawData.h emcGeaRawDataContainer.h emcGeaRawData_dict.h emcGeaRawDataContainer_dict.h 
#libemc_datatypes_la_SOURCES += emcGeaRawData.cc emcGeaRawData_dict.cc emcGeaRawDataContainer.cc emcGeaRawDataContainer_dict.cc 
BUILT_SOURCES += emcGeaRawData_dict.cc
CLEAN_FILES += emcGeaRawData_dict.h emcGeaRawData_dict.cc
#include_HEADERS += 
noinst_HEADERS += emcGeaRawData.h emcGeaRawDataContainer.h
libemc_datatypes_la_SOURCES += emcGeaRawData.cc emcGeaRawData_dict.cc




BUILT_SOURCES += emcGeaDeposit_dict.cc emcGeaDepositv1_dict.cc
CLEAN_FILES += emcGeaDeposit_dict.h emcGeaDeposit_dict.cc emcGeaDepositv1_dict.h emcGeaDepositv1_dict.cc
include_HEADERS += emcGeaDeposit.h emcGeaDepositv1.h
noinst_HEADERS += emcGeaDeposit_dict.h emcGeaDepositv1_dict.h
libemc_datatypes_la_SOURCES += emcGeaDeposit.cc emcGeaDeposit_dict.cc emcGeaDepositv1.cc emcGeaDepositv1_dict.cc

BUILT_SOURCES += emcGeaDepositContainer_dict.cc
CLEAN_FILES += emcGeaDepositContainer_dict.h emcGeaDepositContainer_dict.cc
include_HEADERS += emcGeaDepositContainer.h
noinst_HEADERS += emcGeaDepositContainer_dict.h 
libemc_datatypes_la_SOURCES += emcGeaDepositContainer.cc emcGeaDepositContainer_dict.cc 

BUILT_SOURCES += emcGeaDepositHolder_dict.cc
CLEAN_FILES += emcGeaDepositHolder_dict.h emcGeaDepositHolder_dict.cc
include_HEADERS += emcGeaDepositHolder.h
noinst_HEADERS += emcGeaDepositHolder_dict.h 
libemc_datatypes_la_SOURCES += emcGeaDepositHolder.cc emcGeaDepositHolder_dict.cc 



BUILT_SOURCES += emcGeaTowerContent_dict.cc emcGeaTowerContentv4_dict.cc \
    emcGeaTowerContentv5_dict.cc
CLEAN_FILES += emcGeaTowerContent_dict.h emcGeaTowerContent_dict.cc emcGeaTowerContentv4_dict.h emcGeaTowerContentv4_dict.cc \
    emcGeaTowerContentv5_dict.h emcGeaTowerContentv5_dict.cc
include_HEADERS += emcGeaTowerContent.h emcGeaTowerContentv4.h \
    emcGeaTowerContentv5.h
noinst_HEADERS += emcGeaTowerContent_dict.h emcGeaTowerContentv4_dict.h \
    emcGeaTowerContentv5_dict.h
libemc_datatypes_la_SOURCES += emcGeaTowerContent.cc emcGeaTowerContent_dict.cc emcGeaTowerContentv4.cc emcGeaTowerContentv4_dict.cc \
    emcGeaTowerContentv5.cc emcGeaTowerContentv5_dict.cc

BUILT_SOURCES += emcGeaTowerContainer_dict.cc 
CLEAN_FILES += emcGeaTowerContainer_dict.h emcGeaTowerContainer_dict.cc 
include_HEADERS += emcGeaTowerContainer.h 
noinst_HEADERS += emcGeaTowerContainer_dict.h 
libemc_datatypes_la_SOURCES += emcGeaTowerContainer.cc emcGeaTowerContainer_dict.cc 

BUILT_SOURCES += emcGeaTowerContainervA_dict.cc 
CLEAN_FILES += emcGeaTowerContainervA_dict.h emcGeaTowerContainervA_dict.cc 
include_HEADERS += emcGeaTowerContainervA.h 
noinst_HEADERS += emcGeaTowerContainervA_dict.h 
libemc_datatypes_la_SOURCES += emcGeaTowerContainervA.cc emcGeaTowerContainervA_dict.cc 



BUILT_SOURCES += emcGeaClusterContent_dict.cc
CLEAN_FILES += emcGeaClusterContent_dict.h emcGeaClusterContent_dict.cc 
include_HEADERS += emcGeaClusterContent.h
libemc_datatypes_la_SOURCES += emcGeaClusterContent.cc emcGeaClusterContent_dict.cc 
noinst_HEADERS += emcGeaClusterContent_dict.h 

include_HEADERS += emcGeaClusterContentT.h
libemc_datatypes_la_SOURCES += emcGeaClusterContentT.cc

BUILT_SOURCES += emcGeaClusterContentv6_dict.cc
CLEAN_FILES += emcGeaClusterContentv6_dict.h emcGeaClusterContentv6_dict.cc 
include_HEADERS += emcGeaClusterContentv6.h
libemc_datatypes_la_SOURCES += emcGeaClusterContentv6.cc emcGeaClusterContentv6_dict.cc 
noinst_HEADERS += emcGeaClusterContentv6_dict.h 

BUILT_SOURCES += emcGeaClusterContainer_dict.cc
CLEAN_FILES += emcGeaClusterContainer_dict.h emcGeaClusterContainer_dict.cc
include_HEADERS += emcGeaClusterContainer.h
libemc_datatypes_la_SOURCES += emcGeaClusterContainer.cc emcGeaClusterContainer_dict.cc 
noinst_HEADERS += emcGeaClusterContainer_dict.h 

BUILT_SOURCES += emcGeaClusterContainervA_dict.cc
CLEAN_FILES += emcGeaClusterContainervA_dict.h emcGeaClusterContainervA_dict.cc
include_HEADERS += emcGeaClusterContainervA.h
libemc_datatypes_la_SOURCES += emcGeaClusterContainervA.cc emcGeaClusterContainervA_dict.cc 
noinst_HEADERS += emcGeaClusterContainervA_dict.h 



BUILT_SOURCES += emcGeaTrackContent_dict.cc emcGeaTrackContentv1_dict.cc \
    emcGeaTrackContentv2_dict.cc \
    emcGeaTrackContentv3_dict.cc
CLEAN_FILES += emcGeaTrackContent_dict.h emcGeaTrackContent_dict.cc emcGeaTrackContentv1_dict.h emcGeaTrackContentv1_dict.cc \
    emcGeaTrackContentv2_dict.h emcGeaTrackContentv2_dict.cc \
    emcGeaTrackContentv3_dict.h emcGeaTrackContentv3_dict.cc
include_HEADERS += emcGeaTrackContent.h emcGeaTrackContentv1.h \
    emcGeaTrackContentv2.h \
    emcGeaTrackContentv3.h
noinst_HEADERS += emcGeaTrackContent_dict.h emcGeaTrackContentv1_dict.h \
    emcGeaTrackContentv2_dict.h \
    emcGeaTrackContentv3_dict.h
libemc_datatypes_la_SOURCES += emcGeaTrackContent.cc emcGeaTrackContent_dict.cc emcGeaTrackContentv1.cc emcGeaTrackContentv1_dict.cc \
    emcGeaTrackContentv2.cc emcGeaTrackContentv2_dict.cc \
    emcGeaTrackContentv3.cc emcGeaTrackContentv3_dict.cc

BUILT_SOURCES += emcGeaTrackContainer_dict.cc emcGeaTrackContainervA_dict.cc
CLEAN_FILES += emcGeaTrackContainer_dict.h emcGeaTrackContainer_dict.cc emcGeaTrackContainervA_dict.h emcGeaTrackContainervA_dict.cc
include_HEADERS += emcGeaTrackContainer.h emcGeaTrackContainervA.h
noinst_HEADERS += emcGeaTrackContainer_dict.h emcGeaTrackContainervA_dict.h
libemc_datatypes_la_SOURCES += emcGeaTrackContainer.cc emcGeaTrackContainer_dict.cc emcGeaTrackContainervA.cc emcGeaTrackContainervA_dict.cc



BUILT_SOURCES += EmcGeaContainerImporter_dict.cc
CLEAN_FILES += EmcGeaContainerImporter_dict.h EmcGeaContainerImporter_dict.cc
include_HEADERS += EmcGeaContainerImporter.h
noinst_HEADERS += EmcGeaContainerImporter_dict.h
libemc_datatypes_la_SOURCES += EmcGeaContainerImporter.cc EmcGeaContainerImporter_dict.cc



BUILT_SOURCES += EmcRealContainerImporter_dict.cc
CLEAN_FILES += EmcRealContainerImporter_dict.h EmcRealContainerImporter_dict.cc
include_HEADERS += EmcRealContainerImporter.h
noinst_HEADERS += EmcRealContainerImporter_dict.h
libemc_datatypes_la_SOURCES += EmcRealContainerImporter.cc EmcRealContainerImporter_dict.cc













##############################################################################
###
###  calibration modules
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-calibration
testexternals_calibration_SOURCES = testexternals.C
testexternals_calibration_LDADD = libemc-calibration.la



# emcEmbedAfterBurners is needed by EmcSimuRawDataReCal (what is that?)
libemc_calibration_la_LIBADD = \
  libemc-common.la \
  libemc-datatypes.la \
  -lSubsysReco -lfun4all \
  -lphflag \
  -lemcOMpg -lemcOMbase \
  -lemcEmbedAfterBurners

noinst_HEADERS += \
  mEmcCalibratorModuleLinkDef.h \
  EmcSimuRawDataReCalLinkDef.h \
  mEmcMIPCorrModuleLinkDef.h \
  mEmcMIPCorr3ModuleLinkDef.h \
  mEmcTOFCorr2ModuleLinkDef.h \
  mEmcTOFCorr4ModuleLinkDef.h \
  mEmcTOFCorr5ModuleLinkDef.h \
  mEmcTOFCorr6ModuleLinkDef.h

include_HEADERS += \
  mEmcCalibratorModule.h \
  EmcSimuRawDataReCal.h \
  mEmcMIPCorrModule.h \
  mEmcMIPCorr3Module.h \
  mEmcTOFCorr2Module.h \
  mEmcTOFCorr4Module.h \
  mEmcTOFCorr5Module.h \
  mEmcTOFCorr6Module.h

libemc_calibration_la_SOURCES = \
  mEmcCalibratorModule.C mEmcCalibratorDict.C \
  mEmcCalibratorModulev1.C mEmcCalibratorModulev2.C \
  mEmcReCalibratorModulev1.C \
  EmcSimuRawDataReCal.C EmcSimuRawDataReCalDict.C \
  mEmcMIPCorrModule.C mEmcMIPCorrDict.C \
  mEmcMIPCorr3Module.C mEmcMIPCorr3Dict.C \
  mEmcTOFCorr2Module.C mEmcTOFCorr2Dict.C \
  mEmcTOFCorr4Module.C mEmcTOFCorr4Dict.C \
  mEmcTOFCorr5Module.C \
  mEmcTOFCorr6Module.C \
  mEmcMaskDeadTowers.C \
  mEmcApplyQAToSimu.C

EmcSimuRawDataReCalDict.C: EmcSimuRawDataReCal.h EmcSimuRawDataReCalLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^



# apply QA info, externalized (including the init part)
BUILT_SOURCES += EmcApplyQAToSimu_dict.cc EmcApplyQA_dict.cc
CLEAN_FILES += EmcApplyQAToSimu_dict.h EmcApplyQAToSimu_dict.cc EmcApplyQA_dict.h EmcApplyQA_dict.cc
include_HEADERS += EmcApplyQAToSimu.h EmcApplyQA.h
noinst_HEADERS += EmcApplyQAToSimu_dict.h EmcApplyQA_dict.h 
libemc_calibration_la_SOURCES += EmcApplyQAToSimu.cc EmcApplyQAToSimu_dict.cc EmcApplyQA.cc EmcApplyQA_dict.cc 


# obsolete stuff

#noinst_HEADERS +=

#include_HEADERS +=

#libemc_calibration_la_SOURCES +=








##############################################################################
###
###  reconstruction code
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-reconstruction
testexternals_reconstruction_SOURCES = testexternals.C
testexternals_reconstruction_LDADD = libemc-reconstruction.la



libemc_reconstruction_la_LIBADD = \
  libemc-datatypes.la \
  libemc-common.la \
  -lSubsysReco -lnanoDST -lvtx


noinst_HEADERS += RunToYear.h EmcClusterContainerResurrector.linkdef.h \
  EmcTowerContainerResurrector.linkdef.h


include_HEADERS += \
  EmcCluster.h \
  EmcSectorRec.h \
  EmcGlSectorRec.h \
  EmcScSectorRec.h \
  mEmcClusterizerv0.h \
  mEmcClusterizerv1.h \
  EmcClusterContainerResurrector.h \
  EmcTowerContainerResurrector.h

libemc_reconstruction_la_SOURCES = \
  EmcCluster.cxx \
  EmcSectorRec.cxx \
  EmcGlSectorRec.cxx \
  EmcScSectorRec.cxx \
  mEmcClusterizerv0.C \
  mEmcClusterizerv1.C \
  EmcClusterContainerResurrector.C   EmcClusterContainerResurrector_dict.cc \
  EmcTowerContainerResurrector.C EmcTowerContainerResurrector_dict.cc

BUILT_SOURCES += EmcClusterContainerResurrector_dict.cc \
  EmcTowerContainerResurrector_dict.cc

CLEAN_FILES += EmcClusterContainerResurrector_dict.h EmcClusterContainerResurrector_dict.cc \
  EmcTowerContainerResurrector_dict.h EmcTowerContainerResurrector_dict.cc


# obsolete stuff

noinst_HEADERS += \
  mEmcClusterNewModule.h mEmcClusterNewModuleLinkDef.h

include_HEADERS += \
  mEmcClusterNewModule.h

libemc_reconstruction_la_SOURCES += \
  mEmcClusterNewModule.C mEmcClusterNewDict.C








##############################################################################
###
###  simulation code
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-simulation
testexternals_simulation_SOURCES = testexternals.C
testexternals_simulation_LDADD = libemc-simulation.la

libemc_simulation_la_LDFLAGS = \
        ${AM_LDFLAGS} \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl`

libemc_simulation_la_LIBADD = \
  libemc-simulation2.la \
  libemc-common.la \
  libemc-datatypes.la \
  libemc-calibration.la \
  libemc-reconstruction.la \
  -lSubsysReco \
  -lemcCalib -lemcCalib2 \
  -lWrappers -lphflag \
  -lgea -lbbc

noinst_HEADERS += \
  mEmcCalibTowerLinkDef.h \
  mEmcEventLinkDef.h \
  mEmcGeaClusterEval.h \
  mEmcGeaClusterEval2.h mEmcGeaClusterEval2Module.h mEmcGeaClusterEval2LinkDef.h \
  mEmcGeaEvent.h mEmcGeaEventLinkDef.h \
  mEmcGeaMakeRaw.h mEmcGeaMakeRawLinkDef.h \
  mEmcGeaParams.h mEmcGeaParamsLinkDef.h \
  mEmcGeaTowerEval.h mEmcGeaTowerEvalModule.h mEmcGeaTowerEvalLinkDef.h \
  mEmcGeaTrack.h mEmcGeaTrackLinkDef.h \
  mEmcPerfect.h mEmcPerfectModule.h mEmcPerfectLinkDef.h \
  mEmcToolsLinkDef.h \
  RunToYear.h

include_HEADERS += \
  mEmcCalibTower.h \
  mEmcDefGeom.h mEmcDefGeomModule.h \
  mEmcEvent.h \
  mEmcGeaEventModule.h \
  mEmcGeaParamsModule.h \
  mEmcGeaClusterEvalModule.h \
  mEmcCalibTowerModule.h\
  mEmcEventModule.h\
  mEmcGeaMakeRawModule.h \
  mEmcGeaTrackModule.h \
  mEmcToolsModule.h \
  EmcGetGEA.h \
  emcRecoModuleSimFactory.h

# should EmcGetDCM and EmcGetGEA go to obsolete?
# does not matter, all this stuff will be obsolete soon.
libemc_simulation_la_SOURCES = \
  EmcGetDCM.cc \
  EmcGetGEA.cc \
  mEmcGeaGetHits.C \
  mEmcCalibTower.c mEmcCalibTowerPAM.C mEmcCalibTowerModule.C mEmcCalibTowerDict.C \
  mEmcDefGeom.c mEmcDefGeomPAM.C mEmcDefGeomModule.C mEmcDefGeomDict.C \
  mEmcEvent.c mEmcEventPAM.C mEmcEventModule.C mEmcEventDict.C \
  mEmcGeaClusterEval.c mEmcGeaClusterEvalPAM.C mEmcGeaClusterEvalModule.C mEmcGeaClusterEvalDict.C \
  mEmcGeaClusterEval2.c mEmcGeaClusterEval2PAM.C mEmcGeaClusterEval2Module.C mEmcGeaClusterEval2Dict.C \
  mEmcGeaEvent.c mEmcGeaEventPAM.C mEmcGeaEventModule.C mEmcGeaEventDict.C \
  mEmcGeaMakeCalibTower.C \
  mEmcGeaMakeClusterEvaluation.C \
  mEmcGeaMakeRaw.C mEmcGeaMakeRawPAM.C mEmcGeaMakeRawModule.C mEmcGeaMakeRawDict.C \
  mEmcGeaPatchSimulatedCluster.C \
  mEmcGeaParams.c mEmcGeaParamsPAM.C mEmcGeaParamsModule.C mEmcGeaParamsDict.C \
  mEmcGeaTowerEval.c mEmcGeaTowerEvalPAM.C mEmcGeaTowerEvalModule.C mEmcGeaTowerEvalDict.C \
  mEmcGeaTrack.c mEmcGeaTrackPAM.C mEmcGeaTrackModule.C mEmcGeaTrackDict.C \
  mEmcPerfect.c mEmcPerfectPAM.C mEmcPerfectModule.C mEmcPerfectDict.C \
  mEmcToolsModule.C mEmcToolsDict.C \
  mEmcRecoModuleSimulationYear1.C \
  mEmcRecoModuleSimulationYear3.C \
  emcRecoModuleSimFactory.C emcRecoModuleSimFactoryDict.C

mEmcCalibTowerDict.C: mEmcCalibTowerModule.h mEmcCalibTowerLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcDefGeomDict.C: mEmcDefGeomModule.h mEmcDefGeomLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcEventDict.C: mEmcEventModule.h mEmcEventLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaClusterEvalDict.C: mEmcGeaClusterEvalModule.h mEmcGeaClusterEvalLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaClusterEval2Dict.C: mEmcGeaClusterEval2Module.h mEmcGeaClusterEval2LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaEventDict.C: mEmcGeaEventModule.h mEmcGeaEventLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaMakeRawDict.C: mEmcGeaMakeRawModule.h mEmcGeaMakeRawLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaParamsDict.C: mEmcGeaParamsModule.h mEmcGeaParamsLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaTowerEvalDict.C: mEmcGeaTowerEvalModule.h mEmcGeaTowerEvalLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcGeaTrackDict.C: mEmcGeaTrackModule.h mEmcGeaTrackLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcPerfectDict.C: mEmcPerfectModule.h mEmcPerfectLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcToolsDict.C: mEmcToolsModule.h mEmcToolsLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

emcRecoModuleSimFactoryDict.C: emcRecoModuleSimFactory.h emcRecoModuleSimFactoryLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^




# obsolete stuff

noinst_HEADERS += \
  mEmcDCMToRaw.h mEmcDCMToRawModule.h mEmcDCMToRawLinkDef.h \
  mEmcDCMinput.h mEmcDCMinputModule.h mEmcDCMinputLinkDef.h \
  mEmcDCMoutput.h mEmcDCMoutputModule.h mEmcDCMoutputLinkDef.h \
  mEmcFEMToDCM.h mEmcFEMToDCMLinkDef.h \
  mEmcRawToFEM.h mEmcRawToFEMLinkDef.h \
  mEmcRawToLong.h mEmcRawToLongLinkDef.h

include_HEADERS += \
  mEmcDCMToRawModule.h\
  mEmcFEMToDCMModule.h \
  mEmcRawToFEMModule.h \
  mEmcRawToLongModule.h \
  EmcPutDCM.h \
  EmcPutDCMLong.h \
  EmcPutDCMLongReCal.h \
  mEmcPRDFToRaw.h

libemc_simulation_la_SOURCES += \
  mEmcDCMToRaw.c mEmcDCMToRawPAM.C mEmcDCMToRawModule.C mEmcDCMToRawDict.C \
  mEmcFEMToDCM.c mEmcFEMToDCMPAM.C mEmcFEMToDCMModule.C mEmcFEMToDCMDict.C \
  mEmcRawToFEM.c mEmcRawToFEMPAM.C mEmcRawToFEMModule.C mEmcRawToFEMDict.C \
  EmcPutDCM.cc \
  EmcPutDCMLong.cc \
  EmcPutDCMLongReCal.cc \
  mEmcDCMinput.cc mEmcDCMinputPAM.C mEmcDCMinputModule.C mEmcDCMinputDict.C \
  mEmcDCMoutput.cc mEmcDCMoutputPAM.C mEmcDCMoutputModule.C mEmcDCMoutputDict.C \
  mEmcPRDFToRaw.C mEmcPRDFToRawModule.C \
  mEmcGetDCMModule.C \
  mEmcPutDCMModule.C \
  mEmcPutDCMLongModule.C

mEmcDCMToRawDict.C: mEmcDCMToRawModule.h mEmcDCMToRawLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcDCMinputDict.C: mEmcDCMinputModule.h mEmcDCMinputLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcDCMoutputDict.C: mEmcDCMoutputModule.h mEmcDCMoutputLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcFEMToDCMDict.C: mEmcFEMToDCMModule.h mEmcFEMToDCMLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcRawToFEMDict.C: mEmcRawToFEMModule.h mEmcRawToFEMLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

mEmcRawToLongDict.C: mEmcRawToLongModule.h mEmcRawToLongLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^








##############################################################################
###
###  simulation code: new version
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-simulation2
testexternals_simulation2_SOURCES = testexternals.C
testexternals_simulation2_LDADD = libemc-simulation2.la



libemc_simulation2_la_LIBADD = \
  libemc-calibration.la libemc-reconstruction.la \
  libemc-datatypes.la libemc-common.la \
  -lSubsysReco \
  -lPISARoot \
  -lemcCalib -lemcCalib2 \
  -lvtx

libemc_simulation2_la_SOURCES =



# helper stuff: cached functions for speedup
noinst_HEADERS += CachedFunction.h
libemc_simulation2_la_SOURCES += CachedFunction.cc


### parameters ###
# the geometry
BUILT_SOURCES += emcGeaGeometry_dict.cc
CLEAN_FILES += emcGeaGeometry_dict.h emcGeaGeometry_dict.cc
#include_HEADERS += 
noinst_HEADERS += emcGeaGeometry.h emcGeaGeometry_dict.h 
libemc_simulation2_la_SOURCES += emcGeaGeometry.cc emcGeaGeometry_dict.cc 

# various parameter tables
BUILT_SOURCES += emcGeaParams_dict.cc
CLEAN_FILES += emcGeaParams_dict.h emcGeaParams_dict.cc
#include_HEADERS += 
noinst_HEADERS += emcGeaParams.h emcGeaParams_dict.h 
libemc_simulation2_la_SOURCES += emcGeaParams.cc emcGeaParams_dict.cc 

# and the man who fills them
BUILT_SOURCES += EmcGeaParamsSimMaker_dict.cc
CLEAN_FILES += EmcGeaParamsSimMaker_dict.h EmcGeaParamsSimMaker_dict.cc
#include_HEADERS += 
noinst_HEADERS += EmcGeaParamsSimMaker.h EmcGeaParamsSimMaker_dict.h 
libemc_simulation2_la_SOURCES += EmcGeaParamsSimMaker.cc EmcGeaParamsSimMaker_dict.cc 



### basic data structures ###
# emcGeaDep stuff
BUILT_SOURCES += emcGeaEdep_dict.cc
CLEAN_FILES += emcGeaEdep_dict.h emcGeaEdep_dict.cc
#include_HEADERS += 
noinst_HEADERS += emcGeaEdep.h emcGeaEdep_dict.h 
libemc_simulation2_la_SOURCES += emcGeaEdep.cc emcGeaEdep_dict.cc 

# emcGeaEdepContainer stuf
BUILT_SOURCES += emcGeaEdepContainer_dict.cc
CLEAN_FILES += emcGeaEdepContainer_dict.h emcGeaEdepContainer_dict.cc
#include_HEADERS += 
noinst_HEADERS += emcGeaEdepContainer.h emcGeaEdepContainer_dict.h 
libemc_simulation2_la_SOURCES += emcGeaEdepContainer.cc emcGeaEdepContainer_dict.cc 



### the chain ###
# pisa to internal converter
BUILT_SOURCES += EmcGeaPisa2GeaEdep_dict.cc
CLEAN_FILES += EmcGeaPisa2GeaEdep_dict.h EmcGeaPisa2GeaEdep_dict.cc
#include_HEADERS += 
noinst_HEADERS += EmcGeaPisa2GeaEdep.h EmcGeaPisa2GeaEdep_dict.h 
libemc_simulation2_la_SOURCES += EmcGeaPisa2GeaEdep.cc EmcGeaPisa2GeaEdep_dict.cc 

# EmcGeaTrackSimMaker
BUILT_SOURCES += EmcGeaTrackSimMaker_dict.cc
CLEAN_FILES += EmcGeaTrackSimMaker_dict.h EmcGeaTrackSimMaker_dict.cc
#include_HEADERS += 
noinst_HEADERS += EmcGeaTrackSimMaker.h EmcGeaTrackSimMaker_dict.h 
libemc_simulation2_la_SOURCES += EmcGeaTrackSimMaker.cc EmcGeaTrackSimMaker_dict.cc 

# EmcGeaRawDataSimMaker
BUILT_SOURCES += EmcGeaRawDataSimMaker_dict.cc
CLEAN_FILES += EmcGeaRawDataSimMaker_dict.h EmcGeaRawDataSimMaker_dict.cc
#include_HEADERS += 
noinst_HEADERS += EmcGeaRawDataSimMaker.h EmcGeaRawDataSimMaker_dict.h 
libemc_simulation2_la_SOURCES += EmcGeaRawDataSimMaker.cc EmcGeaRawDataSimMaker_dict.cc 

# EmcGeaTowerSimMaker
BUILT_SOURCES += EmcGeaTowerSimMaker_dict.cc
CLEAN_FILES += EmcGeaTowerSimMaker_dict.h EmcGeaTowerSimMaker_dict.cc
#include_HEADERS += 
noinst_HEADERS += EmcGeaTowerSimMaker.h EmcGeaTowerSimMaker_dict.h 
libemc_simulation2_la_SOURCES += EmcGeaTowerSimMaker.cc EmcGeaTowerSimMaker_dict.cc 

# the simulation2 reco module
BUILT_SOURCES += EmcRecoModuleSimulationYear8_dict.cc
CLEAN_FILES += EmcRecoModuleSimulationYear8_dict.h EmcRecoModuleSimulationYear8_dict.cc
include_HEADERS += EmcRecoModuleSimulationYear8.h
noinst_HEADERS += EmcRecoModuleSimulationYear8_dict.h 
libemc_simulation2_la_SOURCES += EmcRecoModuleSimulationYear8.cc EmcRecoModuleSimulationYear8_dict.cc 








##############################################################################
###
###  real data code
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals-realdata
testexternals_realdata_SOURCES = testexternals.C
testexternals_realdata_LDADD = libemc-realdata.la



libemc_realdata_la_LIBADD = \
  libemc-common.la \
  libemc-calibration.la \
  libemc-reconstruction.la \
  -lSubsysReco \
  -lphflag

noinst_HEADERS += \
  mEmcRealEventModuleLinkDef.h


include_HEADERS += \
  mEmcRecalRecoModuleRealYear3v1.h \
  emcRecoModuleRealFactory.h

libemc_realdata_la_SOURCES = \
  mEmcRecoModuleRealYear2.C \
  emcPatch.C \
  mEmcRecoModuleRealYear3.C \
  mEmcRecoModuleRealYear3v1.C \
  mEmcRecalRecoModuleRealYear3v1.C \
  mEmcRecoModuleRealYear4.C \
  emcRecoModuleRealFactory.C emcRecoModuleRealFactoryDict.C

mEmcRealEventDict.C: mEmcRealEventModule.h mEmcRealEventModuleLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

emcRecoModuleRealFactoryDict.C: emcRecoModuleRealFactory.h emcRecoModuleRealFactoryLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^



# obsolete stuff

#noinst_HEADERS +=

include_HEADERS += \
  mEmcRealEventModule.h 

libemc_realdata_la_SOURCES += \
  mEmcRealEventModule.C \
  mEmcRealEventDict.C








##############################################################################
###
###  old libemc
###
##############################################################################


# test library dependencies
noinst_PROGRAMS += testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD = libemc.la


# -lheader -lnanoDST -lphool -lvtx do not seem to be necessary, 
# but leave it there for now for the sake of compatibility
libemc_la_LIBADD = \
  libemc-datatypes.la \
  libemc-simulation.la \
  libemc-realdata.la \
  -lSubsysReco

#noinst_HEADERS +=


include_HEADERS += \
  emcRecoModuleFactory.h


libemc_la_SOURCES = \
  emcRecoModuleFactory.C emcRecoModuleFactoryDict.C


emcRecoModuleFactoryDict.C: emcRecoModuleFactory.h emcRecoModuleFactoryLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^










%_dict.cc: %.h
	@echo testing for  `echo $^ | sed 's/\.h/.linkdef.h/'`...
	@if [ -f `echo $^ | sed 's/\.h/.linkdef.h/'` ]; then \
		echo rootcint -f $@ -c -P $(INCLUDES) $^ `echo $^ | sed 's/\.h/.linkdef.h/'`; \
		rootcint -f $@ -c -P $(INCLUDES) $^ `echo $^ | sed 's/\.h/.linkdef.h/'`; \
	else \
		echo rootcint -f $@ -c -P $(INCLUDES) $^+; \
		rootcint -f $@ -c -P $(INCLUDES) $^+; \
	fi



%Dict.C: %Wrapper.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %Module.h %ModuleLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	 rm -f *Dict* *dict*
	 rm -f testexternals.C