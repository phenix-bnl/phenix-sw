// $Id: DS3.C,v 1.4 2008/06/09 20:23:20 hpereira Exp $

/*!
	\file DS3.C
	\brief calculates distance from road to track at station 3. Uses muid to extrapolate
*/

#include <PHMuoTracksOut.h>
#include "MWGVersion.h"
#include "Tools.h"

using namespace std;

//___________________________________________________
Float_t Tools::DS3(PHMuoTracksOut* &muo, Int_t idx) 
{ 
  
  // make some check on the PHMuoTracks version because some of the accessors are not
  // valid anymore for latest implementations
  if( MWGVersion::get( muo->GetName() ) >= 10 )
  {
    // print explicit error message
    cout << "Tools::DS3( PHMuoTracksOut, int ) - class version \"" << muo->GetName() << "\" is too recent" << endl;
    cout << "and the method is now obsolete." << endl;
    cout << "use Tools::DS3( PHMuoTracksOut, int, int ) instead." << endl;
    cout << endl;
    return 0;
  }
 
	//track position at station 3
	Float_t X3=muo->get_xpos(3,idx);
	Float_t Y3=muo->get_ypos(3,idx);
	Float_t Z3=muo->get_zpos(3,idx);
	
	//! road position at gap 0
	Float_t X0=muo->get_muID_gap0(0,idx);
	Float_t Y0=muo->get_muID_gap0(1,idx);
	Float_t Z0=muo->get_muID_gap0(2,idx);
	
	// road slopes
	Float_t dxdz0=muo->get_muID_gap0(3,idx);
	Float_t dydz0=muo->get_muID_gap0(4,idx);  
	
	// distance from road to track extrapolated at station3
	Float_t dS3 = sqrt(pow((X3-X0-dxdz0*(Z3-Z0)),2) + pow((Y3-Y0-dydz0*(Z3-Z0)),2));
	return dS3;

}

//___________________________________________________
Float_t Tools::DS3(PHMuoTracksOut* &muo, Int_t idx, Int_t iroad) 
{
	
	//track position at station 3
	Float_t X3=muo->get_xpos(3,idx);
	Float_t Y3=muo->get_ypos(3,idx);
	Float_t Z3=muo->get_zpos(3,idx);
	
	//! road position at gap 0
	Float_t X0=muo->get_muIDOO_gap0(0, iroad, idx);
	Float_t Y0=muo->get_muIDOO_gap0(1, iroad, idx);
	Float_t Z0=muo->get_muIDOO_gap0(2, iroad, idx);
	
	
	// road slopes
	Float_t dxdz0=muo->get_muIDOO_gap0(3, iroad, idx);
	Float_t dydz0=muo->get_muIDOO_gap0(4, iroad, idx);  
	
	// distance from road to track extrapolated at station3
	Float_t dS3 = sqrt(pow((X3-X0-dxdz0*(Z3-Z0)),2) + pow((Y3-Y0-dydz0*(Z3-Z0)),2));
	return dS3;
}
