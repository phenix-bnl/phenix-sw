#include "emcEmbedTofAfterBurnerv1.h"

#include "PHCompositeNode.h"
#include "PHTypedNodeIterator.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"
#include <cstdlib>
#include <cmath>

#include <iostream>

ClassImp(emcEmbedTofAfterBurnerv1)

using namespace std;

//_____________________________________________________________________________
void
emcEmbedTofAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply timing corrections on all the towers
  // of the *dEmcCalibTower* object.

  dEmcCalibTowerWrapper* dEmcCalibTower =
    PHNodeHelper<dEmcCalibTowerWrapper>::getTable("dEmcCalibTower",top);

  if ( dEmcCalibTower ) 
    {
      for ( size_t i = 0; i < dEmcCalibTower->RowCount(); ++i )
	{
	  short iarm  = dEmcCalibTower->get_arm(i);
	  short isect = dEmcCalibTower->get_sector(i);
	  short iy    = dEmcCalibTower->get_ind(1,i);
	  short iz    = dEmcCalibTower->get_ind(0,i);

	  bool sanity = ( iarm>=0 && iarm<2 && 
			  isect>=0 && isect<4 && 
			  iy>=0 && iy<48 && 
			  iz>=0 && iz<96 );

	  if ( !sanity ) 
	    {
	      cerr << "<E> emcEmbedTofAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcCalibTower "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }


	  float tof = dEmcCalibTower->get_tof(i);
	  float ecal = dEmcCalibTower->get_ecal(i);

	  tof = correctTOF(tof, iarm, isect, iy, iz, ecal);

	  dEmcCalibTower->set_tof(i,tof);
	}
    }
  else
    {
      cerr << "<E> "
	   << "emcEmbedTofAfterBurnerv1::apply : could not find dEmcCalibTower"
	   << " No correction applied" << endl;
    }
}

//_____________________________________________________________________________
float
emcEmbedTofAfterBurnerv1::correctTOF(float tof, short iarm, short isect,
				     short iy, short iz, float ecal )
{
  float TOFoffset = -1.*sectoroffset[iarm][isect] 
    - toweroffset[iarm][isect][iy][iz];

  float HADcorr = hadronCorrFactor[iarm][isect];

  float tofEshift = 0.;

  // Functional form of timing shift correction as a function of energy
  // We correct the tof of all towers (that have ecal>1, where no hadron PIDtof)
  // In the real afterburner only the central one is corrected

  if ( ecal > 1. ) 
    {
      tofEshift = 0.17 * (log(ecal) + 0.91) * (log(ecal) + 0.91);
    }

  return (HADcorr * (tof+TOFoffset) - ( HADcorr-1) * meanBbcT0) - tofEshift ;

}


//_____________________________________________________________________________
void
emcEmbedTofAfterBurnerv1::identify(ostream& os) const
{
  os << "<I> emcEmbedTofAfterBurnerv1 - "
     << "RUN-2 TOF After Burner (tower level)" << endl;
}
