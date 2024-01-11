#include "emcEmbedSimEnergyAfterBurnerv1.h"

#include "PHCompositeNode.h"
#include "PHTypedNodeIterator.h"
#include "dEmcCalibTowerWrapper.h"
#include "PHNodeHelper.h"
#include <cstdlib>
#include <iostream>
#include <cassert>
#include "fkinWrapper.h"
#include "emcEmbedUtils.h"
#include "TF1.h"
#include "TRandom.h"
#include "mEmcGeometryModule.h"

ClassImp(emcEmbedSimEnergyAfterBurnerv1)

using namespace std;

//_____________________________________________________________________________
emcEmbedSimEnergyAfterBurnerv1::emcEmbedSimEnergyAfterBurnerv1(const double PercentageOfSmearing) :
  PHAfterBurner(), fEnergyResolutionConstantTerm(0)
{
  fConstantTerm = PercentageOfSmearing;
}

//_____________________________________________________________________________
emcEmbedSimEnergyAfterBurnerv1::~emcEmbedSimEnergyAfterBurnerv1()
{
  delete fEnergyResolutionConstantTerm;
}

//_____________________________________________________________________________
void
emcEmbedSimEnergyAfterBurnerv1::apply(PHCompositeNode* top)
{
  // This will apply constant term energy resolution add-on corrections 
  // on all the towers of the *dEmcCalibTower* object.


  dEmcCalibTowerWrapper* dEmcCalibTower =
    PHNodeHelper<dEmcCalibTowerWrapper>::getTable("dEmcCalibTower",top);

  assert(dEmcCalibTower!=0);

  double random = fEnergyResolutionConstantTerm->GetRandom();

  if ( dEmcCalibTower ) 
    {

      for ( size_t i = 0; i < dEmcCalibTower->RowCount(); ++i )
	{
	  short iarm  = dEmcCalibTower->get_arm(i);
	  short isect = dEmcCalibTower->get_sector(i);
	  // this is only for pbsc
	  if (iarm == 1 && isect < 2) continue;
	  short iy    = dEmcCalibTower->get_ind(1,i);
	  short iz    = dEmcCalibTower->get_ind(0,i);

	  bool sanity = ( iarm>=0 && iarm<2 && 
			  isect>=0 && isect<4 && 
			  iy>=0 && iy<48 && 
			  iz>=0 && iz<96 );

	  if ( !sanity ) 
	    {
	      cerr << "<E> emcEmbedSimEnergyAfterBurnerv1::apply : "
		   << "Incorrect information from dEmcCalibTower "
		   << i << " iarm=" << iarm 
		   << " isect=" << isect
		   << " iy=" << iy 
		   << " iz=" << iz
		   << endl;
	      abort();
	    }

	  int is = mEmcGeometryModule::emcOfflineToEmc(iarm,isect);

	  if ( is < 6 ) // PbSc Only
	    {
	      float oldenergy = dEmcCalibTower->get_ecal(i);

	      //	      float newenergy = oldenergy + oldenergy*
	      //		fConstantTerm*fEnergyResolutionConstantTerm->GetRandom();

	      float newenergy = oldenergy + oldenergy*
		fConstantTerm*random;

	      dEmcCalibTower->set_ecal(i,newenergy);	    
	    }
	}
    }
  else
    {
      cerr << "<E> "
	   << "emcEmbedSimEnergyAfterBurnerv1::apply : could not find dEmcCalibTower"
	   << " No correction applied" << endl;
    }
}

//_____________________________________________________________________________
void
emcEmbedSimEnergyAfterBurnerv1::identify(ostream& os) const
{
  os << "<I> emcEmbedSimEnergyAfterBurnerv1 - "
     << "RUN2 SIMULATED PbSc Energy After Burner (tower level)" << endl;
}

//_____________________________________________________________________________
void
emcEmbedSimEnergyAfterBurnerv1::initialize(PHCompositeNode*)
{

  fEnergyResolutionConstantTerm = 
    new TF1("fEnergyResolutionConstantTerm","gaus",-10,10);

  fEnergyResolutionConstantTerm->SetParameters(1,0,1);

  gRandom->SetSeed(0);

  identify();

  cout << "<I> emcEmbedSimEnergyAfterBurnerv1::initialize - My % of smearing is "
       << fConstantTerm << " - My seed is "
       << gRandom->GetSeed() << endl;
}
