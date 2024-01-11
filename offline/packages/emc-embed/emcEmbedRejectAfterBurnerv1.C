#include "emcEmbedRejectAfterBurnerv1.h"
#include "PHEmcConstants.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"

#include <cstdlib>
#include <iostream>

ClassImp(emcEmbedRejectAfterBurnerv1)

using namespace std;

//_____________________________________________________________________________
void
emcEmbedRejectAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply hot/bad rejection on all the towers
  // of the *dEmcCalibTower* object.
  static const int sw2hwsector[] = { 0, 1, 2, 3, 6, 7, 4, 5 };

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
	      cerr << "emcEmbedRejectAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcCalibTower "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }

	  int hwsector = sw2hwsector[ iarm*4 + isect ];
	  int deadmap = computeDeadmap(hwsector,iz,iy);

	  if ( hwsector > 5 ) 
	    {
	      deadmap |= dEmcCalibTower->get_deadmap(i);
	      dEmcCalibTower->set_deadmap(i,deadmap);
	    }
	  else
	    {
	      dEmcCalibTower->set_warnmap(i,deadmap);
	    }
	}
    }
  else
    {
      cerr << "<ERROR> "
	   << "emcEmbedRejectAfterBurnerv1::apply : could not find dEmcCalibTower"
	   << " No correction applied" << endl;
    }
}

//_____________________________________________________________________________
int 
emcEmbedRejectAfterBurnerv1::computeDeadmap(int hwsector, 
					    short zpos, short ypos)
{
  int ztemp, ytemp;
  unsigned int deadmap = 0;

  ytemp = ypos - 2;
  if ( ytemp>=0 )
    {
      for (int iz=0; iz<3; iz++)
	{
	  ztemp = zpos + iz - 1;
	  if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
	       (RejectList[hwsector][ztemp][ytemp] != 0) )
	      {
		unsigned int map = 1 << iz;
		deadmap |= map;
	      }
	}
    }
  for (int iy=0; iy<3; iy++)
    {
      ytemp = ypos + iy - 1;
      if ( ytemp>=0 && ytemp<EmcYtowers[hwsector] )
	{
	  for (int iz=0; iz<5; iz++)
	    {
	      ztemp = zpos + iz - 2;
	      if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
		   (RejectList[hwsector][ztemp][ytemp] != 0) )
		  {
		    unsigned int map = 1 << (iy*5 + iz + 3);
		    deadmap |= map;
		  }
	    }
	}
    }
  ytemp = ypos + 2;
  if ( ytemp>=0 && ytemp<EmcYtowers[hwsector] )
    {
      for (int iz=0; iz<3; iz++)
	{
	  ztemp = zpos + iz - 1;
	  if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
	       (RejectList[hwsector][ztemp][ytemp] != 0) )
	    {
	      unsigned int map = 1 << (iz + 18);
	      deadmap |= map;
	    }
	}
    }
  return deadmap;
}

//_____________________________________________________________________________
void
emcEmbedRejectAfterBurnerv1::identify(ostream& os) const
{
  os << "emcEmbedRejectAfterBurnerv1::identify - "
     << "RUN2 Hot/Bad Tower Rejection After Burner (tower level)" << endl;
}
