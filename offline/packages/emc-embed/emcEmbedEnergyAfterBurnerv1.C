#include "emcEmbedEnergyAfterBurnerv1.h"

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"
#include <cstdlib>

#include <iostream>

ClassImp(emcEmbedEnergyAfterBurnerv1)

using namespace std;

//_____________________________________________________________________________
void
emcEmbedEnergyAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply energy corrections on all the towers
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
	      cerr << "emcEmbedEnergyAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcCalibTower "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }

	  float escale = tower_scalefactor[iarm][isect][iy][iz];

	  dEmcCalibTower->set_ecal( i, dEmcCalibTower->get_ecal(i) * escale) ;
	}
    }
  else
    {
      cerr << "<ERROR> "
	   << "emcEmbedEnergyAfterBurnerv1::apply : could not find dEmcCalibTower"
	   << " No correction applied" << endl;
    }
}


//_____________________________________________________________________________
void
emcEmbedEnergyAfterBurnerv1::identify(ostream& os) const
{
  os << "<I> emcEmbedEnergyAfterBurnerv1 - "
     << "RUN-2 Energy After Burner (tower level)" << endl;
}
