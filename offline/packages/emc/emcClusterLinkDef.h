#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class emcClusterContent+;
#pragma link C++ function operator << (ostream&, const emcClusterContent&);
#pragma link C++ class emcClusterContentv1+;
#pragma link C++ class emcClusterContentv2+;
#pragma link C++ class emcClusterContentv3+;
#pragma link C++ class emcClusterContentv4+;
#pragma link C++ class emcClusterContentv5+;

#pragma link C++ class emcClusterContainer+;
#pragma link C++ class emcClusterContainerv1+;
#pragma link C++ class emcClusterContainerv2+;
#pragma link C++ class emcClusterContainerv3+;
#pragma link C++ class emcClusterContainerv4+;

#pragma link C++ class emcClusterContentv1S+;
#pragma link C++ class emcClusterContainerT<emcClusterContentv1S>+;
#pragma link C++ typedef emcClusterContainerv1S;

#pragma link C++ class emcClusterContentv1M+;
#pragma link C++ class emcClusterContainerT<emcClusterContentv1M>+;
#pragma link C++ typedef emcClusterContainerv1M;

#pragma link C++ class emcClusterContentv5+;
#pragma link C++ class emcClusterContainerT<emcClusterContentv5>+;
#pragma link C++ typedef emcClusterContainerv5;

#pragma link C++ class emcClusterContentv6+;
#pragma link C++ class emcClusterContainerT<emcClusterContentv6>+;
#pragma link C++ typedef emcClusterContainerv6;

#endif
