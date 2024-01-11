// $Id: DDG0.C,v 1.4 2008/06/09 20:12:14 hpereira Exp $

/*
	\file DG0.C
	\brief calculates angle difference from road to track, calculated at gap 0
*/

#include <iostream>
#include <PHMuoTracksOut.h>
#include <MUTOO.h>
#include <cmath>

#include "MWGVersion.h"
#include "Tools.h"

using namespace std;

//________________________________________________________________________________
Float_t Tools::DDG0( PHMuoTracksOut* &muo, Int_t idx) 
{
 

  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DDG0( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete." << endl;
    cout << "use Tools::DDG0( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }
  
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
  {
    //       std::cout<<"MWGpico::DDG0.C: This data does not seem to have kalman projection to Gap0"<<std::endl
    // 	       <<"Using straight line projection from ST3."<<std::endl;
    lastIndex = 3;
  }

  // retrieve mutr momentum
  Float_t px_mut = muo->get_px(lastIndex,idx);
  Float_t py_mut = muo->get_py(lastIndex,idx);
  Float_t pz_mut = muo->get_pz(lastIndex,idx);  
  Float_t p_mut = p( px_mut, py_mut, pz_mut );
  
  // retrieve muid slope
  Float_t slopex_mui = muo->get_muID_gap0(3,idx);
  Float_t slopey_mui = muo->get_muID_gap0(4,idx);
  Float_t slopez_mui = 1;

  // negate the muid slope to match pz sign
  if( pz_mut < 0 ) {
  
    slopex_mui *= -1;
    slopey_mui *= -1;
    slopez_mui *= -1;
   
  }
   
  Float_t slope_mui = sqrt( 
    MUTOO::SQUARE( slopex_mui )+
    MUTOO::SQUARE( slopey_mui )+
    MUTOO::SQUARE( slopez_mui ) );
  
  Float_t scalar = (px_mut*slopex_mui + py_mut*slopey_mui + pz_mut*slopez_mui )/( p_mut*slope_mui );
  if(scalar>=1) scalar = 1;

  return MUTOO::RAD_TO_DEG*acos(scalar);

}

Float_t Tools::DDG0( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
//       std::cout<<"MWGpico::DDG0.C: This data does not seem to have kalman projection to Gap0"<<std::endl
// 	       <<"Using straight line projection from ST3."<<std::endl;
      lastIndex = 3;
    }

  // retrieve mutr momentum
  Float_t px_mut = muo->get_px(lastIndex,idx);
  Float_t py_mut = muo->get_py(lastIndex,idx);
  Float_t pz_mut = muo->get_pz(lastIndex,idx);  
  Float_t p_mut = p( px_mut, py_mut, pz_mut );
  
  // retrieve muid slope
  Float_t slopex_mui = muo->get_muIDOO_gap0(3,iroad, idx);
  Float_t slopey_mui = muo->get_muIDOO_gap0(4,iroad, idx);
  Float_t slopez_mui = 1;

  // negate the muid slope to match pz sign
  if( pz_mut < 0 ) {
  
    slopex_mui *= -1;
    slopey_mui *= -1;
    slopez_mui *= -1;
   
  }
   
  Float_t slope_mui = sqrt( 
    MUTOO::SQUARE( slopex_mui )+
    MUTOO::SQUARE( slopey_mui )+
    MUTOO::SQUARE( slopez_mui ) );
  
  Float_t scalar = (px_mut*slopex_mui + py_mut*slopey_mui + pz_mut*slopez_mui )/( p_mut*slope_mui );
  if(scalar>=1) scalar = 1;

  return MUTOO::RAD_TO_DEG*acos(scalar);

}
