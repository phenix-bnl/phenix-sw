AUTOMAKE_OPTIONS = foreign

testexternals_SOURCES = testexternals.C

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libemcCalib.la libemcCalib2.la libemcCalibGFX.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall

libemcCalibGFX_la_LIBADD = libemcCalib.la -lpdbcalBase

libemcCalib_la_LIBADD = -lEmcDynamic -lemcDataManager -lphool

libemcCalib2_la_LIBADD = -lEmcStatic -lphool -lTDirectoryHelper libemcCalib.la

libemcCalib2_la_SOURCES = \
  emcBadModules.C \
  emcBadModulesv1.C \
  emcCalibrationDataHelper.C \
  emcDataError.h \
  emcDataProcessor.C \
  emcDataProcessorRun4.C \
  emcDataProcessorv2.C \
  emcDataStorageMap.C \
  emcDCProcessor.C \
  emcDCProcessorv2.C \
  emcDCProcessorv3.C \
  emcFEMList.C \
  emcGainBaseLineCalculator.C \
  emcPacketProcessor.C \
  emcPacketProcessorv1.C \
  emcRawDataProcessor.C \
  emcRawDataProcessorv2.C \
  emcRawDataProcessorv3.C \
  emcTimeStamp.C \
  emcTowerContainer.C \
  emcTowerContainerv1.C \
  emcTowerContainerv1M.C \
  emcTowerContainerv1S.C \
  emcTowerContainerv2.C \
  emcTowerContainerv3.C \
  emcTowerContainerDST.C \
  emcClusterAuxInfo.C \
  emcClusterAuxInfoV1.C \
  emcClusterAuxInfoContainer.C \
  emcClusterAuxInfoContainerV1.C \
  emcTowerContent.C \
  emcTowerContentv1.C \
  emcTowerContentv1M.C \
  emcTowerContentv1S.C \
  emcTowerContentv2.C \
  emcTowerContentv3.C \
  emcTowerContentDST.C \
  Calib2Cint.C	

libemcCalibGFX_la_SOURCES = \
  emcBadNormt.C \
  emcGainEvolution.C \
  emcChannelEvolution.C \
  emcTracedFEMMerger.C \
  emcTracedFEMPlotter.C \
  CalibGFXCint.C

libemcCalib_la_SOURCES = \
  emcCalFEM.C \
  emcCalFEMFactory.C \
  emcCalibratedDataObject.C \
  emcCalibrator.h \
  emcCalibratorFactory.C \
  emcFEMtuple.C \
  emcFEMtupleFactory.C \
  emcTracedFEM.C \
  emcGainFEM.C \
  emcGains.C \
  emcHLRatioFEM.C \
  emcHLRatios.h \
  emcLCTofFEM.C \
  emcLCTofs.C \
  emcMixedDataObject.C \
  emcPedestalFEM.C \
  emcPedestals.h \
  emcQAFEM.C \
  emcQAs.C \
  emcRawDataCalibrator.C \
  emcRawDataCalibratorV1.C \
  emcRawDataCalibratorV2.C \
  emcRejectList.C \
  emcTofT0FEM.C \
  emcTacPedFEM.C \
  emcTacPeds.h \
  emcTracedValue.C \
  emcWalkTofFEM.C \
  emcWalkTofs.C \
  pbscTimingFixes.C \
  CalibCint.C

noinst_HEADERS = CalibLinkDef.h

include_HEADERS = \
  emcBadModules.h \
  emcBadModulesv1.h \
  emcCalFEM.h \
  emcCalFEMFactory.h \
  emcCalibratedDataObject.h \
  emcCalibrationDataHelper.h \
  emcCalibrator.h \
  emcCalibratorFactory.h \
  emcDataProcessor.h \
  emcDataProcessorRun4.h \
  emcDataProcessorv2.h \
  emcDataError.h \
  emcDataStorageMap.h \
  emcDCProcessor.h \
  emcDCProcessorv3.h \
  emcFEMtuple.h \
  emcFEMtupleFactory.h \
  emcTracedFEM.h \
  emcGainFEM.h \
  emcGains.h \
  emcTacPedFEM.h \
  emcTacPeds.h \
  emcHLRatioFEM.h \
  emcHLRatios.h \
  emcLCTofFEM.h \
  emcLCTofs.h \
  emcMixedDataObject.h \
  emcPedestalFEM.h \
  emcPedestals.h \
  emcPedestals5.h \
  emcQAFEM.h \
  emcQAs.h \
  emcPacketProcessor.h \
  emcRawDataCalibrator.h \
  emcRawDataCalibratorV1.h \
  emcRawDataCalibratorV2.h \
  emcRejectList.h \
  emcTofT0FEM.h \
  emcTofT0s.h \
  emcTracedValue.h \
  emcWalkTofFEM.h \
  emcWalkTofs.h \
  pbscTimingFixes.h \
  emcTracedFEMPlotter.h \
  emcTowerContainer.h \
  emcTowerContainerT.h \
  emcTowerContainerv1.h \
  emcTowerContainerv1M.h \
  emcTowerContainerv1S.h \
  emcTowerContainerv2.h \
  emcTowerContainerv3.h \
  emcTowerContainerDST.h \
  emcClusterAuxInfo.h \
  emcClusterAuxInfoV1.h \
  emcClusterAuxInfoContainer.h \
  emcClusterAuxInfoContainerV1.h \
  emcTowerContent.h \
  emcTowerContentv1.h \
  emcTowerContentv1M.h \
  emcTowerContentv1S.h \
  emcTowerContentv2.h \
  emcTowerContentv3.h \
  emcTowerContentDST.h \
  emcTimeStamp.h

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libemcCalib.la \
  libemcCalibGFX.la \
  libemcCalib2.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

CalibCint.C: \
  emcCalibrator.h emcFEMtuple.h emcCalibratorFactory.h \
  emcFEMtuple.h emcFEMtupleFactory.h emcCalFEM.h emcCalFEMFactory.h \
  emcGains.h emcGainFEM.h \
  emcTacPeds.h emcTacPedFEM.h \
  emcTracedFEM.h emcMixedDataObject.h \
  emcCalibratedDataObject.h emcTracedValue.h \
  emcPedestals.h emcPedestals5.h \
  emcPedestalFEM.h emcHLRatios.h emcHLRatioFEM.h \
  emcLCTofs.h emcLCTofFEM.h emcWalkTofs.h emcWalkTofFEM.h \
  emcTofT0s.h emcTofT0FEM.h \
  emcQAs.h emcQAFEM.h \
  emcRawDataCalibrator.h \
  emcRawDataCalibratorV1.h \
  emcRawDataCalibratorV2.h \
  emcRejectList.h \
  pbscTimingFixes.h \
  CalibLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

Calib2Cint.C: \
  emcBadModules.h \
  emcBadModulesv1.h \
  emcCalibrationDataHelper.h \
  emcDataError.h \
  emcDataProcessor.h \
  emcDataProcessorRun4.h \
  emcDataProcessorv2.h \
  emcDataStorageMap.h \
  emcDCProcessor.h \
  emcDCProcessorv2.h \
  emcDCProcessorv3.h \
  emcFEMList.h \
  emcGainBaseLineCalculator.h \
  emcPacketProcessor.h \
  emcPacketProcessorv1.h \
  emcRawDataProcessor.h \
  emcRawDataProcessorv2.h \
  emcRawDataProcessorv3.h \
  emcTimeStamp.h \
  emcTowerContent.h \
  emcTowerContentv1.h \
  emcTowerContentv1M.h \
  emcTowerContentv1S.h \
  emcTowerContentv2.h \
  emcTowerContentv3.h \
  emcTowerContentDST.h \
  emcClusterAuxInfo.h \
  emcClusterAuxInfoV1.h \
  emcClusterAuxInfoContainer.h \
  emcClusterAuxInfoContainerV1.h \
  emcTowerContainer.h \
  emcTowerContainerv1.h \
  emcTowerContainerv1M.h \
  emcTowerContainerv1S.h \
  emcTowerContainerv2.h \
  emcTowerContainerv3.h \
  emcTowerContainerDST.h \
  Calib2LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CalibGFXCint.C: \
	emcTracedFEMMerger.h \
	emcTracedFEMPlotter.h \
	emcGainEvolution.h \
	emcChannelEvolution.h \
	emcBadNormt.h \
	CalibGFXLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Cint*


