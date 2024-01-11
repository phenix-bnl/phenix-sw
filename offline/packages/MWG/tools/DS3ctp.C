// $Id: DS3ctp.C,v 1.4 2008/06/09 20:23:21 hpereira Exp $

/*
\file DS3.C
\brief calculates distance from road to track, calculated at station 3
*/

#include "MWGVersion.h"
#include "Tools.h"
#include <PHMuoTracksOut.h>

using namespace std;

//__________________________________________________
Float_t Tools::DS3ctp(PHMuoTracksOut* &muo, Int_t idx) 
{
  
  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DS3ctp( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << " and the method is now obsolete." << endl;
    cout << "use Tools::DS3ctp( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }
  
  // mutr position
  Float_t X3=muo->get_xpos(3,idx);
  Float_t Y3=muo->get_ypos(3,idx);
  Float_t Z3=muo->get_zpos(3,idx);
  
  // muid position	
  Float_t X0=muo->get_muID_gap0(0,idx);
  Float_t Y0=muo->get_muID_gap0(1,idx);
  Float_t Z0=muo->get_muID_gap0(2,idx);
  
  // distance from road to track at station3 assuming constant theta/phy
  Float_t dS3ctp = sqrt(pow((X3-X0*Z3/Z0),2) + pow((Y3-Y0*Z3/Z0),2));
  return dS3ctp;
  
}

//__________________________________________________
Float_t Tools::DS3ctp(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
  
  // mutr position
  Float_t X3=muo->get_xpos(3,idx);
  Float_t Y3=muo->get_ypos(3,idx);
  Float_t Z3=muo->get_zpos(3,idx);
  
  // muid position	
  Float_t X0=muo->get_muIDOO_gap0(0, iroad, idx);
  Float_t Y0=muo->get_muIDOO_gap0(1, iroad, idx);
  Float_t Z0=muo->get_muIDOO_gap0(2, iroad, idx);
  
  // distance from road to track at station3 assuming constant theta/phy
  Float_t dS3ctp = sqrt(pow((X3-X0*Z3/Z0),2) + pow((Y3-Y0*Z3/Z0),2));
  return dS3ctp;
  
}
