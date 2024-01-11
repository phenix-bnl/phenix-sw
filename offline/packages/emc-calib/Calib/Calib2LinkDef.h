#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class emcBadModules+;
#pragma link C++ function operator << (ostream&, emcBadModules&);
#pragma link C++ class emcBadModulesv1+;

#pragma link C++ class emcPacketProcessor+;
#pragma link C++ class emcPacketProcessorv1+;
#pragma link C++ class emcDataError-!;
#pragma link C++ class emcDataProcessor+;
#pragma link C++ class emcDataProcessorv2+;
#pragma link C++ class emcDataProcessorRun4+;
#pragma link C++ class emcRawDataProcessor+;
#pragma link C++ class emcRawDataProcessorv2+;
#pragma link C++ class emcRawDataProcessorv3+;
#pragma link C++ class emcDCProcessor+;
#pragma link C++ class emcDCProcessorv2+;
#pragma link C++ class emcDCProcessorv3+;

#pragma link C++ class emcTowerContent+;
#pragma link C++ function operator << (ostream&, const emcTowerContent&);
#pragma link C++ class emcTowerContentv1+;
#pragma link C++ class emcTowerContentv2+;
#pragma link C++ class emcTowerContentv3+;
#pragma link C++ class emcTowerContentDST+;

#pragma link C++ class emcTowerContainer+;
#pragma link C++ class emcTowerContainerv1+;
#pragma link C++ class emcTowerContainerv2+;
#pragma link C++ class emcTowerContainerv3+;
#pragma link C++ class emcTowerContainerDST+;

#pragma link C++ class emcClusterAuxInfo+;
#pragma link C++ class emcClusterAuxInfoV1+;
#pragma link C++ class emcClusterAuxInfoContainer+;
#pragma link C++ class emcClusterAuxInfoContainerV1+;

// Objects for simulation
#pragma link C++ class emcTowerContentv1S+;
#pragma link C++ class emcTowerContainerT<emcTowerContentv1S>+;
#pragma link C++ typedef emcTowerContainerv1S;

// For embedding
#pragma link C++ class emcTowerContentv1M+;
#pragma link C++ class emcTowerContainerT<emcTowerContentv1M>+;
#pragma link C++ typedef emcTowerContainerv1M;

#pragma link C++ class emcTimeStamp;
#pragma link C++ class emcCalibrationDataHelper;
#pragma link C++ class emcGainBaseLineCalculator;

#pragma link C++ class emcFEMList;
#pragma link C++ class emcDataStorageMap;

#endif
