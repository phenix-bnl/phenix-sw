// $Id: SvxGeoLinkDef.h,v 1.2 2014/05/02 20:03:54 adare Exp $

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ nestedclasses;
#pragma link C++ nestedtypedefs;
#pragma link C++ class vector<SvxTGeo::GBox>+;
#pragma link C++ class vector<SvxTGeo::GBox>::*;
#pragma link C++ class vector<SvxGeoTrack>+;
#pragma link C++ class vector<SvxGeoTrack>::*;

#pragma link C++ class SvxTGeo+;
#pragma link C++ struct SvxTGeo::GBox+;
#pragma link C++ class SvxGeoTrack+;
#pragma link C++ class SvxGeoHit+;
#pragma link C++ class SvxProj+;

#endif /* __CINT__ */
