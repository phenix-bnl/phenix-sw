AUTOMAKE_OPTIONS = foreign 

lib_LTLIBRARIES = libPgCal.la libPgCalInstance.la

BUILT_SOURCES = \
  testexternals.C

BANKOMAT = perl $(srcdir)/PgBankomat.pl
ROOTIFY =  perl $(srcdir)/PgRootify.pl

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include  \
  -I$(OPT_PHENIX)/include \
  -I`root-config --incdir`

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -L$(OPT_PHENIX)/lib \
  `root-config --libs`

libPgCal_la_LIBADD = \
  -lpdbcalBase \
  -lRDBCodbc \
  -lRDBC \
  -lodbc++

libPgCalInstance_la_SOURCES = PgPostInstantiator.cc

libPgCalInstance_la_LIBADD = \
  libPgCal.la

libPgCal_la_SOURCES = \
  PgPostAccCalibBank.cc \
  PgPostAccCalibBank_dict.C \
  PgPostADCChanBank.cc \
  PgPostADCChanBank_dict.C \
  PgPostApplication.cc  \
  PgPostApplication_dict.C \
  PgPostBankManager.cc \
  PgPostBankManager_dict.C \
  PgPostBankWrapper.cc \
  PgPostBankWrapper_dict.C \
  PgPostBankWrapper2.cc \
  PgPostBankWrapper2_dict.C \
  PgPostBankWrapperManager.cc \
  PgPostBankWrapperManager_dict.C \
  PgPostBankBackupManager.cc \
  PgPostBankBackupManager_dict.C \
  PgPostBankBackupLog.cc \
  PgPostBankBackupLog_dict.C \
  PgPostBankBackupStorage.cc \
  PgPostBankBackupStorage_dict.C \
  PgPostBbcConfBank.cc \
  PgPostBbcConfBank_dict.C \
  PgPostCalBank.cc \
  PgPostCalBank_dict.C \
  PgPostCalBankIterator.cc \
  PgPostCalBankIterator2.cc \
  PgPostCalParametersBank.cc \
  PgPostCalParametersBank_dict.C \
  PgPostCoordinateBank.cc \
  PgPostCoordinateBank_dict.C \
  PgPostDchWireBank.cc \
  PgPostDchWireBank_dict.C \
  PgPostDoubleBank.cc \
  PgPostDoubleBank_dict.C \
  PgPostEmcEScaleTowerBank.cc \
  PgPostEmcEScaleTowerBank_dict.C \
  PgPostEmcEScaleTowerRecalBank.cc \
  PgPostEmcEScaleTowerRecalBank_dict.C \
  PgPostEmcHLRatioVectorBank.cc \
  PgPostEmcHLRatioVectorBank_dict.C \
  PgPostEmcHLRatioBank.cc \
  PgPostEmcHLRatioBank_dict.C \
  PgPostEmcLCTofBank.cc \
  PgPostEmcLCTofBank_dict.C \
  PgPostEmcPedestalBank.cc \
  PgPostEmcPedestalBank_dict.C \
  PgPostEmcPedestal5Bank.cc \
  PgPostEmcPedestal5Bank_dict.C \
  PgPostEmcSecGeomBank.cc \
  PgPostEmcSecGeomBank_dict.C \
  PgPostEmcT0TowerBank.cc \
  PgPostEmcT0TowerBank_dict.C \
  PgPostEmcT0SectorBank.cc \
  PgPostEmcT0SectorBank_dict.C \
  PgPostEmcTofSectorOffsetBank.cc \
  PgPostEmcTofSectorOffsetBank_dict.C \
  PgPostEmcTowerRejectBank.cc \
  PgPostEmcTowerRejectBank_dict.C \
  PgPostEmcTracedValueBank.cc \
  PgPostEmcTracedValueBank_dict.C \
  PgPostEmcWalkTofBank.cc \
  PgPostEmcWalkTofBank_dict.C \
  PgPostEmcTrackMatchBank.cc \
  PgPostEmcTrackMatchBank_dict.C \
  PgPostErtDecodeBank.cc \
  PgPostErtDecodeBank_dict.C\
  PgPostErtLutEmcTileBank.cc \
  PgPostErtLutEmcTileBank_dict.C \
  PgPostErtSMMaskBank.cc \
  PgPostErtSMMaskBank_dict.C \
  PgPostErtSMEffBank.cc \
  PgPostErtSMEffBank_dict.C \
  PgPostFclGainBank.cc \
  PgPostFclGainBank_dict.C \
  PgPostFloatVectorBank.cc \
  PgPostFloatVectorBank_dict.C \
  PgPostHbdAdcBank.cc \
  PgPostHbdAdcBank_dict.C \
  PgPostHbdModuleGainBank.cc \
  PgPostHbdModuleGainBank_dict.C \
  PgPostHelper.cc \
  PgPostIndexBank_dict.C \
  PgPostIndexBank.cc \
  PgPostIntBank.cc \
  PgPostIntBank_dict.C \
  PgPostMapHSBank.cc \
  PgPostMapHSBank_dict.C \
  PgPostMapIntVecFloatBank.cc \
  PgPostMapIntVecFloatBank_dict.C \
  PgPostMapIntIntBank.cc \
  PgPostMapIntIntBank_dict.C \
  PgPostMapIntMapIntIntBank.cc \
  PgPostMapIntMapIntIntBank_dict.C \
  PgPostMpcShapeBank.cc \
  PgPostMpcShapeBank_dict.C \
  PgPostMpcGainCorrBank.cc \
  PgPostMpcGainCorrBank_dict.C \
  PgPostMpcLeakageBank.cc \
  PgPostMpcLeakageBank_dict.C \
  PgPostMpcLedBank.cc\
  PgPostMpcLedBank_dict.C\
  PgPostMpcMapBank.cc \
  PgPostMpcMapBank_dict.C \
  PgPostMpcNoiseBank.cc \
  PgPostMpcNoiseBank_dict.C \
  PgPostMpcPinMapBank.cc \
  PgPostMpcPinMapBank_dict.C \
  PgPostMpcExCoordinateMapBank.cc \
  PgPostMpcExCoordinateMapBank_dict.C \
  PgPostMpcExGeoBank.cc \
  PgPostMpcExGeoBank_dict.C \
  PgPostMpcExHighLowBank.cc \
  PgPostMpcExHighLowBank_dict.C \
  PgPostMpcExHotDeadBank.cc \
  PgPostMpcExHotDeadBank_dict.C \
  PgPostMpcExLayerMIPBank.cc \
  PgPostMpcExLayerMIPBank_dict.C \
  PgPostMpcExMapBank.cc \
  PgPostMpcExMapBank_dict.C \
  PgPostMpcExMinipadMIPBank.cc \
  PgPostMpcExMinipadMIPBank_dict.C \
  PgPostMpcExMinipadMIPCorrectionBank.cc \
  PgPostMpcExMinipadMIPCorrectionBank_dict.C \
  PgPostMpcExMinipadNeighborsBank.cc \
  PgPostMpcExMinipadNeighborsBank_dict.C \
  PgPostMpcExSmearMCMinipadsBank.cc \
  PgPostMpcExSmearMCMinipadsBank_dict.C \
  PgPostMpcExSensorMIPBank.cc \
  PgPostMpcExSensorMIPBank_dict.C \
  PgPostMpcExMipsBank.cc \
  PgPostMpcExMipsBank_dict.C \
  PgPostMpcExOfflinePedestalsBank.cc \
  PgPostMpcExOfflinePedestalsBank_dict.C \
  PgPostMpcExOnlinePedestalsBank.cc \
  PgPostMpcExOnlinePedestalsBank_dict.C \
  PgPostMpcExPedestalShiftBank.cc \
  PgPostMpcExPedestalShiftBank_dict.C \
  PgPostMuiChannelMapBank.cc \
  PgPostMuiChannelMapBank_dict.C \
  PgPostMuiHVMapBank.cc \
  PgPostMuiHVMapBank_dict.C \
  PgPostMuiPanelGeoBank.cc \
  PgPostMuiPanelGeoBank_dict.C \
  PgPostMuiTriggerMLUBank.cc \
  PgPostMuiTriggerMLUBank_dict.C \
  PgPostMuiTubeEffBank.cc \
  PgPostMuiTubeEffBank_dict.C \
  PgPostMuiTubeGeoBank.cc \
  PgPostMuiTubeGeoBank_dict.C \
  PgPostMuRecoParBank.cc \
  PgPostMuRecoParBank_dict.C \
  PgPostMutADAMBank.cc \
  PgPostMutADAMBank_dict.C \
  PgPostMutAlignmentBank.cc \
  PgPostMutAlignmentBank_dict.C \
  PgPostMutCalibBank.cc \
  PgPostMutCalibBank_dict.C \
  PgPostMutCalibOptBank.cc \
  PgPostMutCalibOptBank_dict.C \
  PgPostMutCalibStripBank.cc \
  PgPostMutCalibStripBank_dict.C \
  PgPostMutCalibStrip_v2Bank.cc \
  PgPostMutCalibStrip_v2Bank_dict.C \
  PgPostMutChamberParBank.cc \
  PgPostMutChamberParBank_dict.C \
  PgPostMutChannelMapBank.cc \
  PgPostMutChannelMapBank_dict.C \
  PgPostMutDallasFEMBank.cc \
  PgPostMutDallasFEMBank_dict.C \
  PgPostMutDallasGlinkBank.cc \
  PgPostMutDallasGlinkBank_dict.C \
  PgPostFvtxDeadMapBank.cc \
  PgPostFvtxDeadMapBank_dict.C \
  PgPostFvtxAlignmentBank.cc \
  PgPostFvtxAlignmentBank_dict.C \
  PgPostFvtxAlignmentNumericBank.cc \
  PgPostFvtxAlignmentNumericBank_dict.C \
  PgPostMutDCMMapBank.cc \
  PgPostMutDCMMapBank_dict.C \
  PgPostMutGeomBank.cc \
  PgPostMutGeomBank_dict.C \
  PgPostMutHVDisabledBank.cc \
  PgPostMutHVDisabledBank_dict.C \
  PgPostMutLvdCaBank.cc \
  PgPostMutLvdCaBank_dict.C \
  PgPostMutrgRpcTrkBank.cc \
  PgPostMutrgRpcTrkBank_dict.C \
  PgPostMvdCalibBank.cc \
  PgPostMvdCalibBank_dict.C \
  PgPostMvdCrosstalkBank.cc \
  PgPostMvdCrosstalkBank_dict.C \
  PgPostMvdDeadchannelBank.cc \
  PgPostMvdDeadchannelBank_dict.C \
  PgPostMvdMapBank.cc \
  PgPostMvdMapBank_dict.C \
  PgPostMvdRpBank.cc \
  PgPostMvdRpBank_dict.C \
  PgPostPadBadChBank.cc \
  PgPostPadBadChBank_dict.C \
  PgPostPadBadROCBank.cc \
  PgPostPadBadROCBank_dict.C \
  PgPostPadGeoChamBank.cc \
  PgPostPadGeoChamBank_dict.C \
  PgPostPadGeoParBank.cc \
  PgPostPadGeoParBank_dict.C \
  PgPostPadHVBank.cc \
  PgPostPadHVBank_dict.C \
  PgPostPadROCCalBank.cc \
  PgPostPadROCCalBank_dict.C \
  PgPostPadROCPosBank.cc \
  PgPostPadROCPosBank_dict.C \
  PgPostPadSimGeoChamV2Bank.cc \
  PgPostPadSimGeoChamV2Bank_dict.C \
  PgPostPadSimGeoParV1Bank.cc \
  PgPostPadSimGeoParV1Bank_dict.C \
  PgPostParameterBank.cc \
  PgPostParameterBank_dict.C \
  PgPostParameterErrorBank.cc \
  PgPostParameterErrorBank_dict.C \
  PgPostPisaInterfaceBank.cc \
  PgPostPisaInterfaceBank_dict.C \
  PgPostPmtFitParBank.cc \
  PgPostPmtFitParBank_dict.C \
  PgPostPmtPeakBank.cc \
  PgPostPmtPeakBank_dict.C \
  PgPostRichADCBank.cc \
  PgPostRichADCBank_dict.C \
  PgPostRichParBank.cc \
  PgPostRichParBank_dict.C \
  PgPostRpcDeadNoiseBank.cc \
  PgPostRpcDeadNoiseBank_dict.C \
  PgPostRpcGeometryBank.cc \
  PgPostRpcGeometryBank_dict.C \
  PgPostRxNPBank.cc \
  PgPostRxNPBank_dict.C \
  PgPostRxNPPedBank.cc \
  PgPostRxNPPedBank_dict.C \
  PgPostRxNPSurveyBank.cc \
  PgPostRxNPSurveyBank_dict.C \
  PgPostRxNPSlewBank.cc \
  PgPostRxNPSlewBank_dict.C \
  PgPostSpinGL1pBank.cc \
  PgPostSpinGL1pBank_dict.C \
  PgPostSvxBiasBank.cc \
  PgPostSvxBiasBank_dict.C \
  PgPostSvxDeadMapBank.cc \
  PgPostSvxDeadMapBank_dict.C \
  PgPostSvxStripThresholdBank.cc \
  PgPostSvxStripThresholdBank_dict.C \
  PgPostSvxPixelHotDeadPixelMapBank.cc \
  PgPostSvxPixelHotDeadPixelMapBank_dict.C \
  PgPostSvxPixelHotDeadChipMapBank.cc \
  PgPostSvxPixelHotDeadChipMapBank_dict.C \
  PgPostSvxBeamCenterBank.cc \
  PgPostSvxBeamCenterBank_dict.C \
  PgPostSvxCoordinateOffsetBank.cc \
  PgPostSvxCoordinateOffsetBank_dict.C \
  PgPostSvxDaqErrorMapBank.cc \
  PgPostSvxDaqErrorMapBank_dict.C \
  PgPostTecPedestalBank.cc \
  PgPostTecPedestalBank_dict.C \
  PgPostTRDlikeBank.cc \
  PgPostTRDlikeBank_dict.C \
  PgPostTzrFemMapBank.cc \
  PgPostTzrFemMapBank_dict.C \
  PgPostTzrCalibBank.cc \
  PgPostTzrCalibBank_dict.C \
  PgPostTzrSetupBank.cc \
  PgPostTzrSetupBank_dict.C \
  PgPostZdcLUTBank.cc \
  PgPostZdcLUTBank_dict.C \
  RunToTimePg.cc \
  RunToTimePg_dict.C

include_HEADERS = \
  PgPostApplication.hh \
  PgPostBankManager.hh \
  PgPostCalBank.hh \
  RunToTimePg.hh

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = \
  libPgCal.la \
  libPgCalInstance.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# this generates the standard LinkDef.h files
%_LinkDef.h : 
	 $(ROOTIFY) $@

# keep the LinkDef files in the build area for debugging
.PRECIOUS : %_LinkDef.h

# this generates the classes stored in our postgres DB
PgPost%Bank.hh PgPost%Bank.cc:
	$(BANKOMAT) $@

# this generates the standard dictionaries
%_dict.C : %.hh %_LinkDef.h
	 rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -rf *_dict*
