// $Id: DS0.C,v 1.4 2008/06/19 11:35:43 hpereira Exp $

/*
\file DS0.C
\brief  compute DS0
\author WooJin J. Park, Ming X. Liu
*/

#include "MWGVersion.h"
#include "Tools.h"
#include <PHMuoTracksOut.h>

using namespace std;

//____________________________________________________________
Float_t Tools::DS0(PHMuoTracksOut* &muo, Int_t idx) 
{
  
  Float_t gap0x=-999, gap0y=-999,gap0z=-999, muidrefx=-999,muidrefy=-999,ds0=-999;
  Float_t dxdz=0, dydz=0;
  
  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DS0( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete. It needs to be re-implemented using ( PHMuoTracksOut, int, int )" << endl;
    cout << "as arguments." << endl;
    cout << endl;
    return 0;
  }
  
  //calculate muID road doca@z=0
  gap0x = muo->get_muID_gap0(0,idx);
  gap0y = muo->get_muID_gap0(1,idx);
  gap0z = muo->get_muID_gap0(2,idx);
  dxdz  = muo->get_muID_gap0(3,idx); // cos_theta_x?
  dydz  = muo->get_muID_gap0(4,idx); // cos_theta_y?
  
  //calculate MUID impact parameter @Z=0
  muidrefx = gap0x + dxdz*(0-gap0z);
  muidrefy = gap0y + dydz*(0-gap0z);
  
  ds0 = sqrt(muidrefx*muidrefx + muidrefy*muidrefy);
  
  return ds0;
}

//___________________________________________________
Float_t Tools::DS0(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  Float_t gap0x=-999, gap0y=-999,gap0z=-999, muidrefx=-999,muidrefy=-999,ds0=-999;
  Float_t dxdz=0, dydz=0;
  
  //calculate muID road doca@z=0
  gap0x = muo->get_muIDOO_gap0(0,iroad, idx);
  gap0y = muo->get_muIDOO_gap0(1,iroad, idx);
  gap0z = muo->get_muIDOO_gap0(2,iroad, idx);
  dxdz  = muo->get_muIDOO_gap0(3,iroad, idx); // cos_theta_x?
  dydz  = muo->get_muIDOO_gap0(4,iroad, idx); // cos_theta_y?
  
  //calculate MUID impact parameter @Z=0
  muidrefx = gap0x + dxdz*(0-gap0z);
  muidrefy = gap0y + dydz*(0-gap0z);
  
  ds0 = sqrt(muidrefx*muidrefx + muidrefy*muidrefy);
  
  return ds0;
}
