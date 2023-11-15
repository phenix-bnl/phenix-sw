AUTOMAKE_OPTIONS = foreign 

lib_LTLIBRARIES = libpdbcalBase.la 

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include \
  -I`root-config --incdir`

ROOTIFY  = perl $(srcdir)/PdbRootify.pl

libpdbcalBase_la_LDFLAGS = \
  `root-config --evelibs`
libpdbcalBase_la_LIBADD = \
  -L$(libdir) -L$(OFFLINE_MAIN)/lib -lphool -lphgeo

#please add new classes sorted according to the roman alphabet
libpdbcalBase_la_SOURCES = \
  PdbAccCalib.cc \
  PdbAccCalib_dict.C \
  PdbADCChan.cc \
  PdbADCChan_dict.C \
  PdbApplication.cc \
  PdbApplication_dict.C \
  PdbApplicationFactory.cc \
  PdbApplicationFactory_dict.C \
  PdbBankID.cc \
  PdbBankID_dict.C \
  PdbBankID2.cc \
  PdbBankID2_dict.C \
  PdbBankListIterator.cc \
  PdbBankListIterator_dict.C \
  PdbBankList.cc \
  PdbBankList_dict.C \
  PdbBankManager.cc \
  PdbBankManager_dict.C \
  PdbBankManagerFactory.cc \
  PdbBankManagerFactory_dict.C \
  PdbBbcConf.cc \
  PdbBbcConf_dict.C \
  PdbCalBank.cc \
  PdbCalBank_dict.C \
  PdbCalBankIterator.cc \
  PdbCalBankIterator_dict.C \
  PdbCalChan.cc \
  PdbCalChan_dict.C \
  PdbCalHeader.cc \
  PdbCalHeader_dict.C \
  PdbCalParameters.cc \
  PdbCalParameters_dict.C \
  PdbCoordinate.cc \
  PdbCoordinate_dict.C \
  PdbDchWire.cc \
  PdbDchWire_dict.C \
  PdbDouble.cc \
  PdbDouble_dict.C \
  PdbEmcEScaleTower.cc \
  PdbEmcEScaleTower_dict.C \
  PdbEmcEScaleTowerRecal.cc \
  PdbEmcEScaleTowerRecal_dict.C \
  PdbEmcHLRatio.cc \
  PdbEmcHLRatio_dict.C \
  PdbEmcHLRatioVector.cc \
  PdbEmcHLRatioVector_dict.C \
  PdbEmcLCTof.cc \
  PdbEmcLCTof_dict.C \
  PdbEmcPedestal.cc \
  PdbEmcPedestal_dict.C \
  PdbEmcPedestal5.cc \
  PdbEmcPedestal5_dict.C \
  PdbEmcSecGeom.cc \
  PdbEmcSecGeom_dict.C \
  PdbEmcT0Tower.cc \
  PdbEmcT0Tower_dict.C \
  PdbEmcT0Sector.cc \
  PdbEmcT0Sector_dict.C \
  PdbEmcTrackMatch.cc \
  PdbEmcTrackMatch_dict.C \
  PdbEmcTofSectorOffset.cc \
  PdbEmcTofSectorOffset_dict.C \
  PdbEmcTowerReject.cc \
  PdbEmcTowerReject_dict.C \
  PdbEmcTracedValue.cc \
  PdbEmcTracedValue_dict.C \
  PdbEmcWalkTof.cc \
  PdbEmcWalkTof_dict.C \
  PdbErtDecode.cc \
  PdbErtDecode_dict.C \
  PdbErtLutEmcTile.cc \
  PdbErtLutEmcTile_dict.C \
  PdbErtSMMask.cc \
  PdbErtSMMask_dict.C \
  PdbErtSMEff.cc \
  PdbErtSMEff_dict.C \
  PdbFclGain.cc \
  PdbFclGain_dict.C \
  PdbFloatVector.cc \
  PdbFloatVector_dict.C \
  PdbHbdAdc.cc \
  PdbHbdAdc_dict.C \
  PdbHbdModuleGain.cc \
  PdbHbdModuleGain_dict.C \
  PdbIndex.cc \
  PdbIndex_dict.C \
  PdbInt.cc \
  PdbInt_dict.C \
  PdbMpcExCoordinateMap.cc \
  PdbMpcExCoordinateMap_dict.C \
  PdbMpcExGeo.cc \
  PdbMpcExGeo_dict.C \
  PdbMpcExHighLow.cc \
  PdbMpcExHighLow_dict.C \
  PdbMpcExHotDead.cc \
  PdbMpcExHotDead_dict.C \
  PdbMpcExLayerMIP.cc \
  PdbMpcExLayerMIP_dict.C \
  PdbMpcExMap.cc \
  PdbMpcExMap_dict.C \
  PdbMpcExMinipadMIP.cc \
  PdbMpcExMinipadMIP_dict.C \
  PdbMpcExMinipadMIPCorrection.cc \
  PdbMpcExMinipadMIPCorrection_dict.C \
  PdbMpcExMinipadNeighbors.cc \
  PdbMpcExMinipadNeighbors_dict.C \
  PdbMpcExMips.cc \
  PdbMpcExMips_dict.C \
  PdbMpcExOfflinePedestals.cc \
  PdbMpcExOfflinePedestals_dict.C \
  PdbMpcExOnlinePedestals.cc \
  PdbMpcExOnlinePedestals_dict.C \
  PdbMpcExPedestalShift.cc \
  PdbMpcExPedestalShift_dict.C \
  PdbMpcExSensorMIP.cc \
  PdbMpcExSensorMIP_dict.C \
  PdbMpcExSmearMCMinipads.cc \
  PdbMpcExSmearMCMinipads_dict.C \
  PdbMapHS.cc \
  PdbMapHS_dict.C \
  PdbMapIntVecFloat.cc \
  PdbMapIntVecFloat_dict.C \
  PdbMapIntInt.cc \
  PdbMapIntInt_dict.C \
  PdbMapIntMapIntInt.cc \
  PdbMapIntMapIntInt_dict.C \
  PdbMpcGainCorr.cc \
  PdbMpcGainCorr_dict.C \
  PdbMpcLeakage.cc \
  PdbMpcLeakage_dict.C \
  PdbMpcLed.cc \
  PdbMpcLed_dict.C \
  PdbMpcMap.cc \
  PdbMpcMap_dict.C \
  PdbMpcNoise.cc \
  PdbMpcNoise_dict.C \
  PdbMpcPinMap.cc \
  PdbMpcPinMap_dict.C \
  PdbMpcShape.cc \
  PdbMpcShape_dict.C \
  PdbMuiChannelMap.cc \
  PdbMuiChannelMap_dict.C \
  PdbMuiHVMap.cc \
  PdbMuiHVMap_dict.C \
  PdbMuiPanelGeo.cc \
  PdbMuiPanelGeo_dict.C \
  PdbMuiTriggerMLU.cc \
  PdbMuiTriggerMLU_dict.C \
  PdbMuiTubeEff.cc \
  PdbMuiTubeEff_dict.C \
  PdbMuiTubeGeo.cc \
  PdbMuiTubeGeo_dict.C \
  PdbMuRecoPar.cc \
  PdbMuRecoPar_dict.C \
  PdbMutADAM.cc \
  PdbMutADAM_dict.C \
  PdbMutAlignment.cc \
  PdbMutAlignment_dict.C \
  PdbMutCalib.cc \
  PdbMutCalib_dict.C \
  PdbMutCalibOpt.cc \
  PdbMutCalibOpt_dict.C \
  PdbMutCalibStrip.cc \
  PdbMutCalibStrip_dict.C \
  PdbMutCalibStrip_v2.cc \
  PdbMutCalibStrip_v2_dict.C \
  PdbMutChamberPar.cc \
  PdbMutChamberPar_dict.C \
  PdbMutChannelMap.cc \
  PdbMutChannelMap_dict.C \
  PdbMutDallasFEM.cc \
  PdbMutDallasFEM_dict.C \
  PdbMutDallasGlink.cc \
  PdbMutDallasGlink_dict.C \
  PdbMutDCMMap.cc \
  PdbMutDCMMap_dict.C \
  PdbMutGeom.C \
  PdbMutGeom_dict.C \
  PdbMutHVDisabled.cc \
  PdbMutHVDisabled_dict.C \
  PdbMutLvdCa.cc \
  PdbMutLvdCa_dict.C \
  PdbMutrgRpcTrk.cc \
  PdbMutrgRpcTrk_dict.C \
  PdbFvtxDeadMap.cc \
  PdbFvtxDeadMap_dict.C \
  PdbFvtxAlignment.cc \
  PdbFvtxAlignment_dict.C \
  PdbFvtxAlignmentNumeric.cc \
  PdbFvtxAlignmentNumeric_dict.C \
  PdbMvdCalib.cc \
  PdbMvdCalib_dict.C \
  PdbMvdCrosstalk.cc \
  PdbMvdCrosstalk_dict.C \
  PdbMvdDeadchannel.cc \
  PdbMvdDeadchannel_dict.C \
  PdbMvdMap.cc \
  PdbMvdMap_dict.C \
  PdbMvdRp.cc \
  PdbMvdRp_dict.C \
  PdbNames.cc \
  PdbPadBadCh.cc \
  PdbPadBadCh_dict.C \
  PdbPadGeoCham.cc \
  PdbPadGeoCham_dict.C \
  PdbPadGeoPar.cc \
  PdbPadGeoPar_dict.C \
  PdbPadHV.cc \
  PdbPadHV_dict.C \
  PdbPadBadROC.cc \
  PdbPadBadROC_dict.C \
  PdbPadROCCal.cc \
  PdbPadROCCal_dict.C \
  PdbPadROCPos.cc \
  PdbPadROCPos_dict.C \
  PdbPadSimGeoChamV2.cc \
  PdbPadSimGeoChamV2_dict.C \
  PdbPadSimGeoParV1.cc \
  PdbPadSimGeoParV1_dict.C \
  PdbParameter.cc \
  PdbParameter_dict.C \
  PdbParameterError.cc \
  PdbParameterError_dict.C \
  PdbPisaInterface.cc \
  PdbPisaInterface_dict.C \
  PdbPmtFitPar.cc \
  PdbPmtFitPar_dict.C \
  PdbPmtPeak.cc \
  PdbPmtPeak_dict.C \
  PdbRichADC.cc \
  PdbRichADC_dict.C \
  PdbRichPar.cc \
  PdbRichPar_dict.C \
  PdbRpcDeadNoise.cc \
  PdbRpcDeadNoise_dict.C \
  PdbRpcGeometry.cc \
  PdbRpcGeometry_dict.C \
  PdbRxNP.cc \
  PdbRxNP_dict.C \
  PdbRxNPPed.cc \
  PdbRxNPPed_dict.C \
  PdbRxNPSlew.cc \
  PdbRxNPSlew_dict.C \
  PdbRxNPSurvey.cc \
  PdbRxNPSurvey_dict.C \
  PdbSpinGL1p.cc \
  PdbSpinGL1p_dict.C \
  PdbSvxBias.cc \
  PdbSvxBias_dict.C \
  PdbSvxDeadMap.cc \
  PdbSvxDeadMap_dict.C \
  PdbSvxStripThreshold.cc \
  PdbSvxStripThreshold_dict.C \
  PdbSvxPixelHotDeadPixelMap.cc \
  PdbSvxPixelHotDeadPixelMap_dict.C \
  PdbSvxPixelHotDeadChipMap.cc \
  PdbSvxPixelHotDeadChipMap_dict.C \
  PdbSvxBeamCenter.cc \
  PdbSvxBeamCenter_dict.C \
  PdbSvxCoordinateOffset.cc \
  PdbSvxCoordinateOffset_dict.C \
  PdbSvxDaqErrorMap.cc \
  PdbSvxDaqErrorMap_dict.C \
  PdbTecPedestal.cc \
  PdbTecPedestal_dict.C \
  PdbTRDlike.cc \
  PdbTRDlike_dict.C \
  PdbTzrCalib.cc \
  PdbTzrCalib_dict.C \
  PdbTzrFemMap.cc \
  PdbTzrFemMap_dict.C \
  PdbTzrSetup.cc \
  PdbTzrSetup_dict.C \
  PdbZdcLUT.cc \
  PdbZdcLUT_dict.C \
  PHGenericFactoryT.C \
  RunToTime.cc \
  RunToTime_dict.C \
  RunToTimeFactory.cc \
  RunToTimeFactory_dict.C

#please add new includes sorted according to the roman alphabet
include_HEADERS = \
  Pdb.hh \
  PdbAccCalib.hh \
  PdbADCChan.hh \
  PdbApplication.hh \
  PdbApplicationFactory.hh \
  PdbBankID.hh \
  PdbBankID2.hh \
  PdbBankList.hh \
  PdbBankListIterator.hh \
  PdbBankManager.hh \
  PdbBankManagerFactory.hh \
  PdbBbcConf.hh \
  PdbCalBank.hh \
  PdbCalBankIterator.hh \
  PdbCalChan.hh \
  PdbCalHeader.hh \
  PdbCalParameters.hh \
  PdbClassMap.hh \
  PdbCoordinate.hh \
  PdbDchWire.hh \
  PdbDouble.hh \
  PdbFclGain.hh \
  PdbFloatVector.hh \
  PdbIndex.hh \
  PdbHbdAdc.hh \
  PdbHbdModuleGain.hh \
  PdbEmcEScaleTower.hh \
  PdbEmcEScaleTowerRecal.hh \
  PdbEmcHLRatioVector.hh \
  PdbEmcHLRatio.hh \
  PdbEmcLCTof.hh \
  PdbEmcPedestal.hh \
  PdbEmcPedestal5.hh \
  PdbEmcSecGeom.hh \
  PdbEmcT0Sector.hh \
  PdbEmcT0Tower.hh \
  PdbEmcTrackMatch.hh \
  PdbEmcTofSectorOffset.hh \
  PdbEmcTowerReject.hh \
  PdbEmcTracedValue.hh \
  PdbEmcWalkTof.hh \
  PdbErtDecode.hh \
  PdbErtLutEmcTile.hh \
  PdbErtSMMask.hh \
  PdbErtSMEff.hh \
  PdbInt.hh \
  PdbMapHS.hh \
  PdbMapIntVecFloat.hh \
  PdbMapIntInt.hh \
  PdbMapIntMapIntInt.hh \
  PdbMpcExCoordinateMap.hh \
  PdbMpcExGeo.hh \
  PdbMpcExHighLow.hh \
  PdbMpcExHotDead.hh \
  PdbMpcExLayerMIP.hh \
  PdbMpcExMap.hh \
  PdbMpcExMinipadMIP.hh \
  PdbMpcExMinipadMIPCorrection.hh \
  PdbMpcExMinipadNeighbors.hh \
  PdbMpcExSensorMIP.hh \
  PdbMpcExMips.hh \
  PdbMpcExOnlinePedestals.hh \
  PdbMpcExOfflinePedestals.hh \
  PdbMpcExPedestalShift.hh \
  PdbMpcExSmearMCMinipads.hh \
  PdbMpcGainCorr.hh \
  PdbMpcLeakage.hh \
  PdbMpcLed.hh \
  PdbMpcMap.hh \
  PdbMpcNoise.hh \
  PdbMpcShape.hh \
  PdbMpcPinMap.hh \
  PdbMuiChannelMap.hh \
  PdbMuiHVMap.hh \
  PdbMuiPanelGeo.hh \
  PdbMuiTriggerMLU.hh \
  PdbMuiTubeEff.hh \
  PdbMuiTubeGeo.hh \
  PdbMuRecoPar.hh \
  PdbMutADAM.hh \
  PdbMutAlignment.hh \
  PdbMutCalib.hh \
  PdbMutCalibOpt.hh \
  PdbMutCalibStrip.hh \
  PdbMutCalibStrip_v2.hh \
  PdbMutChamberPar.hh \
  PdbMutChannelMap.hh \
  PdbMutDallasFEM.hh \
  PdbMutDallasGlink.hh \
  PdbMutDCMMap.hh \
  PdbMutGeom.hh \
  PdbMutHVDisabled.hh \
  PdbMutLvdCa.hh \
  PdbMutrgRpcTrk.hh \
  PdbFvtxDeadMap.hh \
  PdbFvtxAlignment.hh \
  PdbFvtxAlignmentNumeric.hh \
  PdbMvdCalib.hh \
  PdbMvdCrosstalk.hh \
  PdbMvdDeadchannel.hh \
  PdbMvdMap.hh \
  PdbMvdRp.hh \
  PdbNames.hh \
  PdbPadBadCh.hh \
  PdbPadBadROC.hh \
  PdbPadGeoPar.hh \
  PdbPadGeoCham.hh \
  PdbPadHV.hh \
  PdbPadROCCal.hh \
  PdbPadROCPos.hh \
  PdbPadSimGeoChamV2.hh \
  PdbPadSimGeoParV1.hh \
  PdbParameter.hh \
  PdbParameterError.hh \
  PdbPisaInterface.hh \
  PdbPmtFitPar.hh \
  PdbPmtPeak.hh \
  PdbRichADC.hh \
  PdbRichPar.hh \
  PdbRpcDeadNoise.hh \
  PdbRpcGeometry.hh \
  PdbRxNP.hh \
  PdbRxNPPed.hh \
  PdbRxNPSlew.hh \
  PdbRxNPSurvey.hh \
  PdbSpinGL1p.hh \
  PdbSvxBias.hh \
  PdbSvxDeadMap.hh \
  PdbSvxStripThreshold.hh \
  PdbSvxPixelHotDeadPixelMap.hh \
  PdbSvxPixelHotDeadChipMap.hh \
  PdbSvxBeamCenter.hh \
  PdbSvxCoordinateOffset.hh \
  PdbSvxDaqErrorMap.hh \
  PdbTecPedestal.hh \
  PdbTRDlike.hh \
  PdbTzrCalib.hh \
  PdbTzrFemMap.hh \
  PdbTzrSetup.hh \
  PdbZdcLUT.hh \
  PHGenericFactoryT.h \
  RunToTime.hh \
  RunToTimeFactory.hh

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = \
  libpdbcalBase.la

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

# this generates the standard dictionary
%_dict.C : %.hh %_LinkDef.h
	 rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

#
# Some Root dependencies have to be put in 'by hand'.
# Do not remove or modify this comment. PdbNewClass depends on it.
#

PdbBankManagerFactory_dict.C : PHGenericFactoryT.h PdbBankManagerFactory.hh PdbBankManagerFactory_LinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

RunToTimeFactory_dict.C: PHGenericFactoryT.h RunToTimeFactory.hh RunToTimeFactory_LinkDef.h
	rootcint -f $@ -c $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *_LinkDef.h
	rm -f *_dict*
