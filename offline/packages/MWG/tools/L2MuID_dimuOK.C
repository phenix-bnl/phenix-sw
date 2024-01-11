// $Id: L2MuID_dimuOK.C,v 1.7 2012/09/14 04:39:50 slash Exp $

/*!
	\file L2MuID_dimuOK.C	
	\brief emulates the level2 muid requirements on the muid offline roads
	is a temporary solution in waiting for the offline to level2 matching algorithm
*/

#include <assert.h>
#include <MuonUtil.h>
#include <PHCompositeNode.h>
#include <PHMuoTracksOut.h>
#include <MWGVersion.h>
#include <RunNumberRanges.h>

#include "Tools.h"

using namespace std;

//_______________________________________________________
bool Tools::L2MuID_dimuOK(int idimu, PHMuoTracksOut* muo, Int_t RUN)
{

  assert( MWGVersion::get( muo->GetName() ) < 10 );
  
  Int_t idx0 = muo->get_ditrkIndex(0,idimu);
  Int_t idx1 = muo->get_ditrkIndex(1,idimu);
  Int_t fpattern0=muo->get_muIDhits(idx0);
  Int_t fpattern1=muo->get_muIDhits(idx1);
  
  Float_t dx0=muo->get_muID_gap0(3,idx0);
  Float_t dy0=muo->get_muID_gap0(4,idx0);
  Float_t dx1=muo->get_muID_gap0(3,idx1);
  Float_t dy1=muo->get_muID_gap0(4,idx1);
  
  //return Tools::L2MuID_dimuOK( fpattern0, fpattern1, dx0, dy0, dx1, dy1, RUN );
  return Tools::L2MuID_dimuOK( fpattern0, fpattern1, dx0, dx1, dy0, dy1, RUN );
}


//_______________________________________________________
bool Tools::L2MuID_dimuOK(
  int idx0, int iroad0, 
  int idx1, int iroad1,
  PHMuoTracksOut* muo, Int_t RUN)
{
 
  if( iroad0 < 0 || iroad1 < 0 ) return false;
  
  Int_t fpattern0=muo->get_muIDOOhits( iroad0, idx0);
  Int_t fpattern1=muo->get_muIDOOhits( iroad1, idx1);
  
  Float_t dx0=muo->get_muIDOO_gap0(3, iroad0, idx0);
  Float_t dy0=muo->get_muIDOO_gap0(4, iroad0, idx0);
  
  Float_t dx1=muo->get_muIDOO_gap0(3, iroad1, idx1);
  Float_t dy1=muo->get_muIDOO_gap0(4, iroad1, idx1);

  //return Tools::L2MuID_dimuOK( fpattern0, fpattern1, dx0, dy0, dx1, dy1, RUN );
  return Tools::L2MuID_dimuOK( fpattern0, fpattern1, dx0, dx1, dy0, dy1, RUN );

  
}

//_______________________________________________________
bool Tools::L2MuID_dimuOK(Int_t idhits0, Int_t idhits1, 
				Float_t gap0_dxdz0, Float_t gap0_dxdz1,
				Float_t gap0_dydz0, Float_t gap0_dydz1,
				Int_t RUN)
{
	//=== set the parameters
	Float_t MinSlope	 = tan( 12.0*M_PI/180.0);
	Float_t DimuAngle	= 19.2; 
	Int_t	 HitPattern = 256; 
	Int_t	 NbHits		 = 8;
  
	if( RUN == RUN4 ) {
		MinSlope	 = tan( 12.0*M_PI/180.0);
		DimuAngle	= 19.2;
		HitPattern = 256;
		NbHits		 = 8;
	}
	if( RUN == RUN5 ) {
		MinSlope	 = tan( 12.0*M_PI/180.0);
		DimuAngle	= 19.2;
		HitPattern = 256;
		NbHits		 = 8;
	}

	//=== First requirement : road depth
	if (! (idhits0>=HitPattern && idhits1>=HitPattern) ) return false;

	//=== Second requirement : road number of hits
	Int_t nbits0 = 0, nbits1=0;
	for (Int_t i=0; i<10;i++) {
		if ( (idhits0 & (1 << i)) != 0) nbits0 += 1;
		if ( (idhits1 & (1 << i)) != 0) nbits1 += 1;
	
	}
	if (! (nbits0>=NbHits && nbits1>=NbHits) ) return false;

	//=== Third requirement : road slope
	Float_t slope0 = sqrt(MUTOO::SQUARE(gap0_dxdz0) + MUTOO::SQUARE(gap0_dydz0));
	Float_t slope1 = sqrt(MUTOO::SQUARE(gap0_dxdz1) + MUTOO::SQUARE(gap0_dydz1));
	if (! (slope0>=MinSlope && slope1>=MinSlope) ) return false;
	
	//=== Fourth requirement : dimuon opening angle
	Float_t cost = 
    (gap0_dxdz0*gap0_dxdz1 + gap0_dydz0*gap0_dydz1 + 1)/(
    sqrt( MUTOO::SQUARE(gap0_dxdz1) + MUTOO::SQUARE(gap0_dydz1) + 1 )*
    sqrt( MUTOO::SQUARE(gap0_dxdz0) + MUTOO::SQUARE(gap0_dydz0) + 1 ));
    
	Float_t Angle = (180/M_PI)*acos(cost);
	return (Angle >= DimuAngle);

}
