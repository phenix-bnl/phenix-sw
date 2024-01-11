#include "emcEmbedSimTofAfterBurnerv1.h"

#include "PHCompositeNode.h"
#include "PHTypedNodeIterator.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"
#include <cstdlib>
#include "mEmcGeometryModule.h"
#include "gsl/gsl_const_mks.h"
#include <iostream>
#include <cassert>
#include "fkinWrapper.h"
#include "emcEmbedUtils.h"

ClassImp(emcEmbedSimTofAfterBurnerv1)

using namespace std;

// Brief description about simulated TOF (from Ana Note 145):
//
// Prior to embedding, the simulated EMCal TOF has to be modified to ressemble the real one:
// 
// * Flash time correction: the simulated TOF in simulated DST is a physical time, i.e. 
//   the flight time given by PISA. For real events, this flight path ($\sim$17 ns) has been subtracted, 
//   so it must be also subtracted for simulated events. This is done here.
//
// * BBC T0 correction: the real TOF, for any given run, is not centered at zero, but 
//   at the mean BBC T0 value for this run. The simulated TOF, corrected by flight path above, 
//   would be centered at zero. So we correct for BBC T0 (adding it). 
//   This is done before merging real and simulated events, in mEmcEmbedRun2.C.


//_____________________________________________________________________________
emcEmbedSimTofAfterBurnerv1::emcEmbedSimTofAfterBurnerv1() : 
  PHAfterBurner(), fSimulatedGeometry(0)
{
}

//_____________________________________________________________________________
emcEmbedSimTofAfterBurnerv1::~emcEmbedSimTofAfterBurnerv1()
{
  delete fSimulatedGeometry;
}

//_____________________________________________________________________________
void
emcEmbedSimTofAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply timing corrections on all the towers
  // of the *dEmcCalibTower* object.

  static const float crazy_big = GSL_CONST_MKS_DAY;

  static PHPoint origin(0,0,0);
  static const double c = GSL_CONST_MKS_SPEED_OF_LIGHT*1E-7; // cm.ns-1

  dEmcCalibTowerWrapper* dEmcCalibTower =
    PHNodeHelper<dEmcCalibTowerWrapper>::getTable("dEmcCalibTower",top);

  assert(dEmcCalibTower!=0);

//   fkinWrapper* fkin = 
//     PHNodeHelper<fkinWrapper>::getTable("fkin",top);
//   assert(fkin!=0);
//   float simul_vtx, ptot_input;
//   check_fkin(fkin,simul_vtx,ptot_input);
//   cout << simul_vtx << endl;

  if ( dEmcCalibTower ) 
    {
      static const int SECTORS[] = { 0,1,2,3,6,7,5,4 };
      // to convert "emcal-offline" convention to that
      // used "emcal-offline-in-mEmcGeometry" one...
      // Did you say mess ?

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
	      cerr << "<E> emcEmbedSimTofAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcCalibTower "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }
	  
	  float x,y,z;

	  int is = SECTORS[iarm*4+isect];
	  int nx, ny;
	  fSimulatedGeometry->GetSectorDim(is,nx,ny);

	  fSimulatedGeometry->GetTowerPosGlobal( is, iy*nx+iz,
						 x,y,z );
	  
	  PHPoint tower(x,y,z);
	  
	  float distance = tower.distanceToPoint(origin);

	  assert(distance>=0);

	  double tshift = distance/c;

// 	  tshift -= 0.1; // ns
// 	  // sorry for the hard-coded value, but as the slewing correction
// 	  // made in response code is slightly incorrect, we correct
// 	  // it...

	  float tof = dEmcCalibTower->get_tof(i);

	  if ( fabs(tof) < 1E-9 ) 
	    {
	      // if tof==0 it means simulated response time
	      // did not cross the discriminator threshold,
	      // i.e. timing is non-existent for this tower.
	      dEmcCalibTower->set_tof(i,crazy_big);
	    }
	  else
	    {
	      tof -= tshift;
	      dEmcCalibTower->set_tof(i,tof);

// 	      // Now let's "worsen" the TOF behaviour of our simulated towers
// 	      // as a function of the energy as found for the real data 
	      
// 	      float tofEshift = 0. ;
// 	      float ecal = dEmcCalibTower->get_ecal(i);

// 	      if (ecal < 0.4)
// 		{
// 		  tofEshift = -0.0039 - 0.076/ecal + 0.019/(ecal*ecal);
// 		}
// 	      else if (ecal < 1.0)
// 		{
// 		  tofEshift = 0.061 - 0.57/ecal + 0.12/(ecal*ecal);
// 		}
// 	      else
// 		{
// 		  tofEshift = 0.17 * (log(ecal) + 0.91) * (log(ecal) + 0.91);
// 		}
	      
// 	      tof+=tofEshift;
// 	      dEmcCalibTower->set_tof(i,tof);
	      
// 	      //cout << "<I> TOF(ns) recentered from: " << old_tof << " to " << tof << endl;
 	    }

	}
    }
  else
    {
      cerr << "<E> "
	   << "emcEmbedSimTofAfterBurnerv1::apply : could not find dEmcCalibTower"
	   << " No correction applied" << endl;
    }
}

//_____________________________________________________________________________
void
emcEmbedSimTofAfterBurnerv1::identify(ostream& os) const
{
  os << "<I> emcEmbedSimTofAfterBurnerv1 - "
     << "RUN-2 SIMULATED TOF After Burner (tower level)" << endl;
}

//_____________________________________________________________________________
void
emcEmbedSimTofAfterBurnerv1::initialize(PHCompositeNode*)
{
  fSimulatedGeometry = new mEmcGeometryModule(mEmcGeometryModule::kPISA);
}
