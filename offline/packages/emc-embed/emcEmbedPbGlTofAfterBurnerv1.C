#include "emcEmbedPbGlTofAfterBurnerv1.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "PHNodeHelper.h"
#include "VtxOut.h"
#include <cstdlib>
#include <cassert>
#include <iostream>
#include "PHPoint.h"

ClassImp(emcEmbedPbGlTofAfterBurnerv1)

//_____________________________________________________________________________
  emcEmbedPbGlTofAfterBurnerv1::emcEmbedPbGlTofAfterBurnerv1() : 
    emcEmbedPbGlTofAfterBurnerv1()
{
}

//_____________________________________________________________________________
emcEmbedPbGlTofAfterBurnerv1::~emcEmbedPbGlTofAfterBurnerv1()
{
}

//_____________________________________________________________________________
void
emcEmbedPbGlTofAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply reject list on all the clusters
  static const int sw2hwsector[] = { 0, 1, 2, 3, 6, 7, 4, 5 };

  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt =
    PHNodeHelper<dEmcClusterLocalExtWrapper>::getTable
    ("dEmcClusterLocalExt",top);
 
  assert(dEmcClusterLocalExt!=0);

  VtxOut* vtxOut = 
    PHNodeHelper<VtxOut>::getTable
    ("VtxOut",top);

  assert(vtxOut!=0);

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
	      cerr << "emcEmbedPbGlTofAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcClusterLocalExt "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }

	  int hwsector = sw2hwsector[ iarm*4 + isect ];

	  if ( is >= 6 )
	    {
	      PHPoint vertex(vtxOut->get_Vertex());

	      float x = vertex.getX()-dEmcClusterLocalExt->get_xyz(0,i);
	      float y = vertex.getY()-dEmcClusterLocalExt->get_xyz(1,i);
	      float z = vertex.getZ()-dEmcClusterLocalExt->get_xyz(2,i);
	      
	      float bad_actual = sqrt(x*x+y*y+y*y);
	      float good_actual = sqrt(x*x+y*y+z*z);

	      float tshift = -bad_actual + good_actual;

	      float tof = tshift + dEmcClusterLocalExt->get_tof(i);

	      dEmcClusterLocalExt->set_tof(i,tof);	      
	    }

	}
    }
  else
    {
      cerr << "<ERROR> "
	   << "emcEmbedPbGlTofAfterBurnerv1::apply : could not find dEmcClusterLocalExt"
	   << " No correction applied" << endl;
    }
}


//_____________________________________________________________________________
void
emcEmbedPbGlTofAfterBurnerv1::identify(ostream& os) const
{
  os << "emcEmbedPbGlTofAfterBurnerv1 - "
     << "RUN2 PbGl ToF After Burner (cluster level)" << endl;
}

//_____________________________________________________________________________
void 
emcEmbedPbGlTofAfterBurnerv1::initialize(PHCompositeNode*)
{
}
