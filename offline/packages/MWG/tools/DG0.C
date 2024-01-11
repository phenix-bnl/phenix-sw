// $Id: DG0.C,v 1.7 2008/06/19 11:35:33 hpereira Exp $

/*
	\file DG0.C
	\brief calculates distance from road to track, calculated at gap 0
*/

#include <iostream>
#include <PHMuoTracksOut.h>
#include <cmath>
#include <MUTOO.h>

#include "MWGVersion.h"
#include "Tools.h"

using namespace std;

//_______________________________________________________
Float_t Tools::DG0( PHMuoTracksOut* &muo, Int_t idx) 
{

  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DG0( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete." << endl;
    cout << "use Tools::DG0( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }
  
  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
  lastIndex = 3;

  Float_t x_mut = muo->get_xpos(lastIndex,idx);
  Float_t y_mut = muo->get_ypos(lastIndex,idx);    
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,idx)/muo->get_pz(lastIndex,idx);
  Float_t dydz_mut = muo->get_py(lastIndex,idx)/muo->get_pz(lastIndex,idx);  
    
  // muid point at gap 0
  Float_t x_mui=muo->get_muID_gap0(0, idx);
  Float_t y_mui=muo->get_muID_gap0(1, idx);
  Float_t z_mui=muo->get_muID_gap0(2, idx);
  
  return sqrt( 
    MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
    MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
  );
    
}

//_______________________________________________________
Float_t Tools::DG0( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
  lastIndex = 3;

  Float_t x_mut = muo->get_xpos(lastIndex,idx);
  Float_t y_mut = muo->get_ypos(lastIndex,idx);    
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,idx)/muo->get_pz(lastIndex,idx);
  Float_t dydz_mut = muo->get_py(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t x_mui=muo->get_muIDOO_gap0(0, iroad, idx);
  Float_t y_mui=muo->get_muIDOO_gap0(1, iroad, idx);
  Float_t z_mui=muo->get_muIDOO_gap0(2, iroad, idx);

  return sqrt( 
    MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
    MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
  );
}
 
//____________________________________________________________
Float_t Tools::DG0x( PHMuoTracksOut* &muo, Int_t idx) 
{
  
  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DG0x( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete." << endl;
    cout << "use Tools::DG0x( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }

  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
      lastIndex = 3;
    }

  Float_t x_mut = muo->get_xpos(lastIndex,idx);
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t x_mui=muo->get_muID_gap0(0, idx);
  Float_t z_mui=muo->get_muID_gap0(2, idx);

  return x_mui - x_mut - dxdz_mut*(z_mui - z_mut);
}

//____________________________________________________________
Float_t Tools::DG0x( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  
  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
      lastIndex = 3;
    }

  Float_t x_mut = muo->get_xpos(lastIndex,idx);
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t x_mui=muo->get_muIDOO_gap0(0, iroad, idx);
  Float_t z_mui=muo->get_muIDOO_gap0(2, iroad, idx);

  return x_mui - x_mut - dxdz_mut*(z_mui - z_mut);
}

//____________________________________________________________
Float_t Tools::DG0y( PHMuoTracksOut* &muo, Int_t idx) 
{
  
  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DG0y( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete." << endl;
    cout << "use Tools::DG0x( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }

  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
      lastIndex = 3;
    }

  Float_t y_mut = muo->get_ypos(lastIndex,idx);
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dydz_mut = muo->get_py(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t y_mui=muo->get_muID_gap0(1, idx);
  Float_t z_mui=muo->get_muID_gap0(2, idx);

  return y_mui - y_mut - dydz_mut*(z_mui - z_mut);
}

//____________________________________________________________
Float_t Tools::DG0y( PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
      lastIndex = 3;
    }

  Float_t y_mut = muo->get_ypos(lastIndex,idx);
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dydz_mut = muo->get_py(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t y_mui=muo->get_muIDOO_gap0(1, iroad, idx);
  Float_t z_mui=muo->get_muIDOO_gap0(2, iroad, idx);

  return y_mui - y_mut - dydz_mut*(z_mui - z_mut);
}
