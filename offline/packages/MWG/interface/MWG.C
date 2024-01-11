// $Id: MWG.C,v 1.9 2014/01/05 06:46:33 slash Exp $

/*!
	\file MWG.C
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.9 $
  \date    $Date: 2014/01/05 06:46:33 $
*/

#include <cmath>

#include "MWG.h"
#include "MWGVersion.h"
#include "PHdiMuoTracksv11.h"
#include "PHMuoTracksv11.h"
#include "PHdiMuoTracksv16.h"
#include "PHMuoTracksv16.h"

//____________________________________________________________
float MWG::get_mass( 
  float px1, float py1, float pz1,
  float px2, float py2, float pz2 )
{
     
   float E1 = sqrt(MUMASS*MUMASS + px1*px1 + py1*py1 + pz1*pz1);
   float E2 = sqrt(MUMASS*MUMASS + px2*px2 + py2*py2 + pz2*pz2);
   float E = E1 + E2;
   float ptot_square = 
    (px1+px2)*(px1+px2) + 
    (py1+py2)*(py1+py2) +
    (pz1+pz2)*(pz1+pz2);
    
   return sqrt(E*E  - ptot_square  );

}

//____________________________________________________________
PHMuoTracksOut* MWG::newPHMuoTracksFvtx( void )
{ 
  PHMuoTracksOut* out = new PHMuoTracksv16();
  std::cout << "MWG::newPHMuoTracksFvtx - using class " << out->GetName() << " (version: " << MWGVersion::get( out->GetName() ) << ")" << std::endl;
  return out;
}

//____________________________________________________________
PHMuoTracksOut* MWG::newPHMuoTracks( void )
{ 
  PHMuoTracksOut* out = new PHMuoTracksv11(); 
  std::cout << "MWG::newPHMuoTracks - using class " << out->GetName() << " (version: " << MWGVersion::get( out->GetName() ) << ")" << std::endl;
  return out;
}

//____________________________________________________________
PHMuoTracksOut* MWG::newPHdiMuoTracks( void )
{ 
  PHMuoTracksOut* out = new PHdiMuoTracksv11(); 
  std::cout << "MWG::newPHdiMuoTracks - using class " << out->GetName() << " (version: " << MWGVersion::get( out->GetName() ) << ")" << std::endl;
  return out;
}

//____________________________________________________________
PHMuoTracksOut* MWG::newPHdiMuoTracksFvtx( void )
{
  PHMuoTracksOut* out = new PHdiMuoTracksv16();
  std::cout << "MWG::newPHdiMuoTracksFvtx - using class " << out->GetName() << " (version: " << MWGVersion::get( out->GetName() ) << ")" << std::endl;
  return out;
}
