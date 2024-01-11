#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class emcCalibrator-! ;
#pragma link C++ class emcCalibratorFactory-!;
#pragma link C++ class emcRawDataCalibrator-! ;
#pragma link C++ class emcRawDataCalibratorV1-! ;
#pragma link C++ class emcRawDataCalibratorV2-! ;
#pragma link C++ class emcMixedDataObject- ;
#pragma link C++ class emcCalibratedDataObject- ;
#pragma link C++ class emcFEMtuple-! ;
#pragma link C++ class emcFEMtupleFactory-! ;
#pragma link C++ class emcCalFEM-! ;
#pragma link C++ class emcCalFEMFactory-! ;
#pragma link C++ function operator << (ostream&, const emcMixedDataObject&);
#pragma link C++ function operator << (ostream&, const emcCalibratedDataObject&);

// Gains related classes
#pragma link C++ class emcTracedValue-!;
#pragma link C++ class emcGains-!;
#pragma link C++ class emcGainFEM-!;
#pragma link C++ class emcTracedFEM-! ;
#pragma link C++ function operator << (ostream&, const emcTracedValue&);

// HL Ratio related classes
#pragma link C++ class emcHLRatios-!;
#pragma link C++ class emcHLRatioFEM-!;

// TOF related classes 
#pragma link C++ class emcLCTofs-!;
#pragma link C++ class emcLCTofFEM-!;
#pragma link C++ class emcWalkTofs-!;
#pragma link C++ class emcWalkTofFEM-!;
#pragma link C++ class emcTofT0s-!;
#pragma link C++ class emcTofT0FEM-! ;
#pragma link C++ class emcTacPeds-!;
#pragma link C++ class emcTacPedFEM-!;
#pragma link C++ class pbscTimingFixes-!;

// Pedestal related classes
#pragma link C++ class emcPedestals-!;
#pragma link C++ class emcPedestals5-!;
#pragma link C++ class emcPedestalFEM-!;

// QA related classes
#pragma link C++ class emcQAs-! ;
#pragma link C++ class emcQAFEM-! ;
#pragma link C++ class emcRejectList-! ;

#endif
