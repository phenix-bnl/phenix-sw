#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class EmcDynamicData-! ;
#pragma link C++ class EMCalFEE-! ;
#pragma link C++ class emcDataFormatter-! ;
#pragma link C++ class emcRawDataAccessor-! ;
#pragma link C++ struct FEMlimits-! ;
#pragma link C++ struct SuperModule-! ;
#pragma link C++ struct Reference-! ;
#pragma link C++ struct EmcData-! ;
#pragma link C++ struct cells-! ;
#pragma link C++ struct evtInfo-! ;
#pragma link C++ class emcConfigurationFile-! ;

#pragma link C++ class emcDataObject- ;
#pragma link C++ class emcRawDataObject- ;     
#pragma link C++ function operator << (ostream&, const emcRawDataObject&);


#endif
