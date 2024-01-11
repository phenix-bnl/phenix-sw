#include "emcEmbedRejectAfterBurnerv2.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "PHNodeHelper.h"
#include <cstdlib>

#include <iostream>

ClassImp(emcEmbedRejectAfterBurnerv2)

using namespace std;

//_____________________________________________________________________________
void
emcEmbedRejectAfterBurnerv2::apply(PHCompositeNode* top)
{
  // This will apply reject list on all the clusters
  static const int sw2hwsector[] = { 0, 1, 2, 3, 6, 7, 4, 5 };

  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt =
    PHNodeHelper<dEmcClusterLocalExtWrapper>::getTable
    ("dEmcClusterLocalExt",top);

  if ( dEmcClusterLocalExt ) 
    {
      for ( size_t i = 0; i < dEmcClusterLocalExt->RowCount(); ++i )
	{
	  short iarm  = dEmcClusterLocalExt->get_arm(i);
	  short isect = dEmcClusterLocalExt->get_sector(i);
	  short iy    = dEmcClusterLocalExt->get_ind(1,i);
	  short iz    = dEmcClusterLocalExt->get_ind(0,i);

	  bool sanity = ( iarm>=0 && iarm<2 && 
			  isect>=0 && isect<4 && 
			  iy>=0 && iy<48 && 
			  iz>=0 && iz<96 );

	  if ( !sanity ) 
	    {
	      cerr << "emcEmbedRejectAfterBurnerv2::apply : "
		   << "Incorrect information from dEmcClusterLocalExt "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }

	  int hwsector = sw2hwsector[ iarm*4 + isect ];
	  int deadmap = computeDeadmap(hwsector,iz,iy);

	  if ( hwsector > 5 ) // we update the DEADMAP for PbGl 
	    {
	      deadmap |= dEmcClusterLocalExt->get_deadmap(i);
	      dEmcClusterLocalExt->set_deadmap(i,deadmap);
	    }
	  else // and clear and set the WARNMAP for PbSc
	    {
	      dEmcClusterLocalExt->set_warnmap(i,0); // fixme: is this really needed ?? (reset before if ??)
	      dEmcClusterLocalExt->set_warnmap(i,deadmap);
	    }
	}
    }
  else
    {
      cerr << "<ERROR> "
	   << "emcEmbedRejectAfterBurnerv2::apply : could not find dEmcClusterLocalExt"
	   << " No correction applied" << endl;
    }
}


//_____________________________________________________________________________
void
emcEmbedRejectAfterBurnerv2::identify(ostream& os) const
{
  os << "<I> emcEmbedRejectAfterBurnerv2 - "
     << "RUN-2 Hot/Bad Tower Rejection After Burner (cluster level)" << endl;
}
