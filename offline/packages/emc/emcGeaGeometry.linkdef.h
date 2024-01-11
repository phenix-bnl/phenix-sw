// 
// $Id: emcGeaGeometry.linkdef.h,v 2.1 2008/04/29 02:20:34 mazsi Exp $ 
// 
// Linkdef file for EmcGeometry
// 

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class emc_geometry_t;
#pragma link C++ class vector< emc_geometry_t >;
#pragma link C++ class vector< vector< emc_geometry_t > >;
#pragma link C++ class vector< vector< vector< emc_geometry_t > > >;
#pragma link C++ class emcGeaGeometry;

#endif
